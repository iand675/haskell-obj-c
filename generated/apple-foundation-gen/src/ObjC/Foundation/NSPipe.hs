{-# LANGUAGE DataKinds #-}
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
  , fileHandleForReadingSelector
  , fileHandleForWritingSelector
  , pipeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ pipe@
pipe :: IO (Id NSPipe)
pipe  =
  do
    cls' <- getRequiredClass "NSPipe"
    sendClassMessage cls' pipeSelector

-- | @- fileHandleForReading@
fileHandleForReading :: IsNSPipe nsPipe => nsPipe -> IO (Id NSFileHandle)
fileHandleForReading nsPipe =
  sendMessage nsPipe fileHandleForReadingSelector

-- | @- fileHandleForWriting@
fileHandleForWriting :: IsNSPipe nsPipe => nsPipe -> IO (Id NSFileHandle)
fileHandleForWriting nsPipe =
  sendMessage nsPipe fileHandleForWritingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pipe@
pipeSelector :: Selector '[] (Id NSPipe)
pipeSelector = mkSelector "pipe"

-- | @Selector@ for @fileHandleForReading@
fileHandleForReadingSelector :: Selector '[] (Id NSFileHandle)
fileHandleForReadingSelector = mkSelector "fileHandleForReading"

-- | @Selector@ for @fileHandleForWriting@
fileHandleForWritingSelector :: Selector '[] (Id NSFileHandle)
fileHandleForWritingSelector = mkSelector "fileHandleForWriting"

