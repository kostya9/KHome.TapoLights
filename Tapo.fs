module KHome.Tapo

open System
open System.Collections.Generic
open System.Net.Http
open System.Net.Http.Headers
open System.Net.Http.Json
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading.Tasks
open FSharp.Control.Tasks.V2

module API =
    let nowMillis() = (DateTime.UtcNow - DateTime(1970, 1, 1)).TotalMilliseconds |> round |> int64
    
    type TapoRequest<'P> = { Method: string; Params: 'P; RequestTimeMils: int64 }
    
    let makeTapoRequest<'TParams> (method: string) (parameters: 'TParams): TapoRequest<'TParams> =
        { Method = method; Params = parameters; RequestTimeMils = nowMillis() }

    /// You are supposed to access this via the active pattern 
    type TapoResponse<'R> = { [<JsonPropertyName("error_code")>] ErrorCode: int; Result: 'R }
        
    let private serializerOptions = JsonSerializerOptions(JsonSerializerDefaults.Web)

    let serialize<'TValue> (obj: 'TValue) = JsonSerializer.SerializeToUtf8Bytes (obj, serializerOptions)
    let deserialize<'TValue> serialized =
        let mutable jsonUtfReader = Utf8JsonReader(ReadOnlySpan<byte>(serialized));
        JsonSerializer.Deserialize<'TValue> (&jsonUtfReader, serializerOptions)

    let postAsync<'TRequest, 'TResult> (httpClient: HttpClient) (request: TapoRequest<'TRequest>) =
        task {
            let serialized = serialize request
            let content = new ByteArrayContent(serialized)
            content.Headers.ContentType <- MediaTypeHeaderValue("application/json")
            
            let! resp = httpClient.PostAsync("", content)
            let! deserialized = resp.Content.ReadFromJsonAsync<TapoResponse<'TResult>>(serializerOptions)
            
            return deserialized
        }

    type RetriedResult<'T> =
        | Success of 'T
        | Error of int
        
    let postWithRetries sendRequest =
        task {
            let maxRetryCount = 3
            let retryableErrorCode = -1301 // Something like Too Many Requests
            
            let rec postWithRetries sendRequest retryCount =
                if retryCount > maxRetryCount
                then failwith $"Could not send request after {maxRetryCount} retries (errCode={retryableErrorCode})"
                else
                    task {
                        try
                            let! response = sendRequest()
                        
                            return!
                                match response with
                                | Success result -> Task.FromResult(result)
                                | Error errCode when errCode = retryableErrorCode ->
                                    Console.WriteLine($"Retrying after errCode {errCode}...")
                                    postWithRetries sendRequest (retryCount + 1)
                                | Error errCode -> failwith $"received errorCode = {errCode}"
                   
                        with
                        | :? OperationCanceledException ->
                            Console.WriteLine($"Retrying after OperationCanceledException...")
                            return! postWithRetries sendRequest (retryCount + 1)
                        | e -> return! failwith $"failed due to unknown exception {e}"
                    }
                    
            return! postWithRetries sendRequest 0
        }
        
    let postAuthenticatedAsync<'TRequest, 'TResult> (httpClient: HttpClient) token (request: TapoRequest<'TRequest>) =
        task {
            let serialized = serialize request
            let content = new ByteArrayContent(serialized)
            content.Headers.ContentType <- MediaTypeHeaderValue("application/json")
            let! resp = httpClient.PostAsync($"?token={token}", content)
            let! deserialized = resp.Content.ReadFromJsonAsync<TapoResponse<'TResult>>(serializerOptions)
            
            return deserialized
        }

    let (|SuccessfulTapoResponse|FailedTapoResponse|) (tapoResponse: TapoResponse<'T>) =
        if tapoResponse.ErrorCode = 0
        then SuccessfulTapoResponse tapoResponse.Result
        else FailedTapoResponse tapoResponse.ErrorCode

module SetDeviceInfo =
    type DeviceInfo = {
        Color : {| Hue: int; Saturation: int |} option
        Brightness: int option
    }
    
    type Params = IDictionary<string, obj>
    type Result = unit
    
    let makeRequest (parameters: DeviceInfo) =
        
        let parametersDictionary: Params =
            seq {
                   ("brightness", parameters.Brightness)
                   ("hue", parameters.Color |> Option.map (fun c -> c.Hue))
                   ("saturation", parameters.Color |> Option.map (fun c -> c.Saturation))
            }
            |> Seq.map (fun (k,v) -> Option.map (fun vSome -> (k, vSome :> obj)) v)
            |> Seq.choose id
            |> dict
        
        API.makeTapoRequest "set_device_info" parametersDictionary

module Handshake =  
    type Params = { Key: string }
    type Result = { Key: string }
    
    let makeRequest (parameters: Params) =
        API.makeTapoRequest "handshake" parameters

module LoginDevice =
    type Params = { Username: string; Password: string  }
    type Result = { Token: string; }
    
    let makeRequest (parameters: Params) =
        API.makeTapoRequest "login_device" parameters
    
module SecurePassthrough =
    type Params = { Request: string }
    type Result = { Response: string }
    
    let makeRequest (parameters: Params) =
        API.makeTapoRequest "securePassthrough" parameters

let private toHandshake publicKey =
    let publicKeyStr = Convert.ToBase64String(publicKey)
    let publicKeyWrapped = $"-----BEGIN PUBLIC KEY-----\n{publicKeyStr}\n-----END PUBLIC KEY-----\n"

    Handshake.makeRequest { Key = publicKeyWrapped }
    
let private securePassthroughEncode (encryptor: ICryptoTransform) request  =
    let requestSerializedBytes = 
        API.serialize request
    
    let secured =
        encryptor.TransformFinalBlock(requestSerializedBytes, 0, requestSerializedBytes.Length)
        |> Convert.ToBase64String
    
    SecurePassthrough.makeRequest { Request = secured }
    
let private securePassthroughDecode<'TResult> (decryptor: ICryptoTransform) (response: SecurePassthrough.Result)  =
    let responseBytes =
            response.Response
            |> Convert.FromBase64String
    
    let deserialized =
        decryptor.TransformFinalBlock(responseBytes, 0, responseBytes.Length)
        |> API.deserialize<API.TapoResponse<'TResult>>
    
    deserialized

type AuthenticatedTapoDevice = private {
    Encryptor: ICryptoTransform
    Decryptor: ICryptoTransform
    Token: string
    Client: HttpClient
}

let authenticate (ip: string) (username: string) (password: string) =
    task {
        let client = new HttpClient()
        client.Timeout <- TimeSpan.FromSeconds(5.0)
        client.BaseAddress <- Uri($"http://{ip}/app")
        
        let key = RSA.Create(1024)
        let publicKey = key.ExportSubjectPublicKeyInfo()
        
        let! handshakeResponse =
            toHandshake publicKey
            |> API.postAsync<_, Handshake.Result> client
        
        let handshakeResult =
            match handshakeResponse with
            | API.SuccessfulTapoResponse result -> result
            | API.FailedTapoResponse errCode -> failwith $"received errorCode = {errCode} on handshake"
        
        let encryptionParts =
            key.Decrypt(Convert.FromBase64String(handshakeResult.Key), RSAEncryptionPadding.Pkcs1)
        
        let key = [|
            for i in 0..15 do
                yield encryptionParts.[i]
        |]
        
        let iv = [|
            for i in 0..15 do
                yield encryptionParts.[i + 16]
        |]
        
        let aes = Aes.Create()
        let encryptor = aes.CreateEncryptor(key, iv)
        let decryptor = aes.CreateDecryptor(key, iv)
        
        let toLower (s: string) = s.ToLower()
        
        let hashedUsernameHexBytes =
            SHA1.HashData(username |> Encoding.UTF8.GetBytes)
            |> Convert.ToHexString
            |> toLower
            |> Encoding.UTF8.GetBytes
        
        let shaUsernameBase64 = Convert.ToBase64String(hashedUsernameHexBytes, Base64FormattingOptions.InsertLineBreaks)
        
        let passwordBase64 = Convert.ToBase64String(password |> Encoding.UTF8.GetBytes, Base64FormattingOptions.InsertLineBreaks)
        
        let! loginResponseSecured =
            LoginDevice.makeRequest { Username = shaUsernameBase64; Password = passwordBase64 }
            |> securePassthroughEncode encryptor
            |> API.postAsync<_, SecurePassthrough.Result> client
        
        let loginResponse =
            match loginResponseSecured with
            | API.SuccessfulTapoResponse result -> securePassthroughDecode<LoginDevice.Result> decryptor result
            | API.FailedTapoResponse errCode -> failwith $"received errorCode = {errCode} on secured login"
        
        return
            match loginResponse with
            | API.SuccessfulTapoResponse result -> ({
                Client = client
                Token = result.Token
                Encryptor = encryptor
                Decryptor = decryptor
            })
            | API.FailedTapoResponse errorCode -> failwith $"received errorCode = {errorCode} on login"
    }


type SetColorOptions = {
    Brightness: int
    Hue: int
    Saturation: int
}
let setColor device setColorOptions =
    task {
        let securePassthroughWithRetries request = 
                API.postWithRetries (fun () -> task {
                    let! secureResponse = API.postAuthenticatedAsync<_, SecurePassthrough.Result> device.Client device.Token request
                    
                    return
                        match secureResponse with
                        | API.SuccessfulTapoResponse result ->
                            
                            let decoded = securePassthroughDecode<SetDeviceInfo.Result> device.Decryptor result 
                            match decoded with
                            | API.SuccessfulTapoResponse result -> API.Success (result)
                            | API.FailedTapoResponse errCode -> API.Error errCode
                            
                        | API.FailedTapoResponse errCode -> API.Error errCode
                })
        
        let! securedResponse =
            SetDeviceInfo.makeRequest
                {
                    Brightness = Some setColorOptions.Brightness
                    Color = Some {| Hue = setColorOptions.Hue; Saturation = setColorOptions.Saturation |}
                }
            |> securePassthroughEncode device.Encryptor
            |> API.postAuthenticatedAsync<_, SecurePassthrough.Result> device.Client device.Token
        
        ()
    }