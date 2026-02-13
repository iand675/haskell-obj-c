{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSendMessageAttachment@.
module ObjC.Intents.INSendMessageAttachment
  ( INSendMessageAttachment
  , IsINSendMessageAttachment(..)
  , attachmentWithAudioMessageFile
  , audioMessageFile
  , attachmentWithAudioMessageFileSelector
  , audioMessageFileSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ attachmentWithAudioMessageFile:@
attachmentWithAudioMessageFile :: IsINFile audioMessageFile => audioMessageFile -> IO (Id INSendMessageAttachment)
attachmentWithAudioMessageFile audioMessageFile =
  do
    cls' <- getRequiredClass "INSendMessageAttachment"
    sendClassMessage cls' attachmentWithAudioMessageFileSelector (toINFile audioMessageFile)

-- | @- audioMessageFile@
audioMessageFile :: IsINSendMessageAttachment inSendMessageAttachment => inSendMessageAttachment -> IO (Id INFile)
audioMessageFile inSendMessageAttachment =
  sendMessage inSendMessageAttachment audioMessageFileSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attachmentWithAudioMessageFile:@
attachmentWithAudioMessageFileSelector :: Selector '[Id INFile] (Id INSendMessageAttachment)
attachmentWithAudioMessageFileSelector = mkSelector "attachmentWithAudioMessageFile:"

-- | @Selector@ for @audioMessageFile@
audioMessageFileSelector :: Selector '[] (Id INFile)
audioMessageFileSelector = mkSelector "audioMessageFile"

