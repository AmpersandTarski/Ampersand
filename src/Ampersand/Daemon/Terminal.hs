{-# LANGUAGE CPP #-}

{- ORMOLU_DISABLE -}
-- | Cross-platform operations for manipulating terminal console windows.
-- _Acknoledgements_: This is mainly copied from Neil Mitchells ghcid.
module Ampersand.Daemon.Terminal(
    terminalTopmost,
    withWindowIcon, WindowIcon(..), setWindowIcon
    ) where
import Ampersand.Basics

#if defined(mingw32_HOST_OS)
import Data.Bits
import Graphics.Win32.Misc
import Graphics.Win32.Window
import Graphics.Win32.Message
import Graphics.Win32.GDI.Types
import System.Win32.Types


wM_GETICON :: WindowMessage
-- wM_SETICON = 0x0080
wM_GETICON = 0x007F

-- iCON_BIG, iCON_SMALL :: WPARAM
-- iCON_BIG = 1
-- iCON_SMALL = 0

#ifdef x86_64_HOST_ARCH
#define CALLCONV ccall
#else
#define CALLCONV stdcall
#endif

foreign import CALLCONV unsafe "windows.h GetConsoleWindow"
    getConsoleWindow :: IO HWND

foreign import CALLCONV unsafe "windows.h SetWindowPos"
    setWindowPos :: HWND -> HWND -> Int -> Int -> Int -> Int -> Word32 -> IO Bool
#endif

-- | Raise the current terminal on top of all other screens, if you can.
terminalTopmost :: IO ()
#if defined(mingw32_HOST_OS)
terminalTopmost = do
    wnd <- getConsoleWindow
    _ <- setWindowPos wnd hWND_TOPMOST 0 0 0 0 (sWP_NOMOVE .|. sWP_NOSIZE)
    return ()
#else
terminalTopmost = return ()
#endif

data WindowIcon = IconOK | IconWarning | IconError

-- | Change the window icon to green, yellow or red depending on whether the file was errorless, contained only warnings or contained at least one error.
setWindowIcon :: WindowIcon -> IO ()
#if defined(mingw32_HOST_OS)
setWindowIcon x = do
    ico <- return $ case x of
        IconOK -> iDI_ASTERISK
        IconWarning -> iDI_EXCLAMATION
        IconError -> iDI_HAND
    icon <- loadIcon Nothing ico
    wnd <- getConsoleWindow
    -- SMALL is the system tray, BIG is the taskbar and Alt-Tab screen
    _ <- sendMessage wnd wM_SETICON iCON_SMALL $ fromIntegral $ castPtrToUINTPtr icon
    _ <- sendMessage wnd wM_SETICON iCON_BIG   $ fromIntegral $ castPtrToUINTPtr icon
    return ()
#else
setWindowIcon _ = return ()
#endif

-- | Run an operation in which you call setWindowIcon
withWindowIcon :: RIO env a -> RIO env a
#if defined(mingw32_HOST_OS)
withWindowIcon act = do
    wnd <- liftIO $ getConsoleWindow
    icoBig <- liftIO $ sendMessage wnd wM_GETICON iCON_BIG 0
    icoSmall <- liftIO $ sendMessage wnd wM_GETICON iCON_SMALL 0
    act `finally` do
        _ <- liftIO $ sendMessage wnd wM_SETICON iCON_BIG icoBig
        _ <- liftIO $ sendMessage wnd wM_SETICON iCON_SMALL icoSmall
        return ()
#else
withWindowIcon act = act
#endif
