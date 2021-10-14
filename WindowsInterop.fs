module KHome.WindowsInterop

    open System.Runtime.InteropServices

    type HDC = int
    type HWND = int
    type COLORREF = uint
    
    [<Struct>]
    [<StructLayout(LayoutKind.Sequential)>]
    type POINT = { x: int; y: int }
    
    [<DllImport("User32", ExactSpelling = true, SetLastError = true, ThrowOnUnmappableChar = true)>]
    extern HDC GetDC(HWND hWnd)
    
    [<DllImport("User32", ExactSpelling = true, SetLastError = true, ThrowOnUnmappableChar = true)>]
    extern int ReleaseDC(HWND hWnd, HDC hdc)
    
    [<DllImport("User32", ExactSpelling = true, SetLastError = true, ThrowOnUnmappableChar = true)>]
    extern bool GetCursorPos(POINT& lpPoint)
    
    [<DllImport("Gdi32", ExactSpelling = true, SetLastError = true, ThrowOnUnmappableChar = true)>]
    extern COLORREF GetPixel(HDC hdc, int x, int y)
    
    