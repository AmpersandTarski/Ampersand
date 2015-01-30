module Database.Design.Ampersand.Prototype.GenFrontend (doGenFrontend) where

import Database.Design.Ampersand.FSpec.FSpec

doGenFrontend :: FSpec -> IO ()
doGenFrontend _ =
 do { putStrLn "Generating new frontend.." 
    ; return ()
    }