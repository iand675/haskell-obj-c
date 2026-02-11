{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPipe@.
module ObjC.Foundation.NSPipe
  ( NSPipe
  , IsNSPipe(..)
  , pipe
  , fileHandleForReading
  , fileHandleForWriting
  , pipeSelector
  , fileHandleForReadingSelector
  , fileHandleForWritingSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ pipe@
pipe :: IO (Id NSPipe)
pipe  =
  do
    cls' <- getRequiredClass "NSPipe"
    sendClassMsg cls' (mkSelector "pipe") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fileHandleForReading@
fileHandleForReading :: IsNSPipe nsPipe => nsPipe -> IO (Id NSFileHandle)
fileHandleForReading nsPipe  =
  sendMsg nsPipe (mkSelector "fileHandleForReading") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fileHandleForWriting@
fileHandleForWriting :: IsNSPipe nsPipe => nsPipe -> IO (Id NSFileHandle)
fileHandleForWriting nsPipe  =
  sendMsg nsPipe (mkSelector "fileHandleForWriting") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pipe@
pipeSelector :: Selector
pipeSelector = mkSelector "pipe"

-- | @Selector@ for @fileHandleForReading@
fileHandleForReadingSelector :: Selector
fileHandleForReadingSelector = mkSelector "fileHandleForReading"

-- | @Selector@ for @fileHandleForWriting@
fileHandleForWritingSelector :: Selector
fileHandleForWritingSelector = mkSelector "fileHandleForWriting"

