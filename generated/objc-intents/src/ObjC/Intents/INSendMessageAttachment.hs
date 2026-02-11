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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ attachmentWithAudioMessageFile:@
attachmentWithAudioMessageFile :: IsINFile audioMessageFile => audioMessageFile -> IO (Id INSendMessageAttachment)
attachmentWithAudioMessageFile audioMessageFile =
  do
    cls' <- getRequiredClass "INSendMessageAttachment"
    withObjCPtr audioMessageFile $ \raw_audioMessageFile ->
      sendClassMsg cls' (mkSelector "attachmentWithAudioMessageFile:") (retPtr retVoid) [argPtr (castPtr raw_audioMessageFile :: Ptr ())] >>= retainedObject . castPtr

-- | @- audioMessageFile@
audioMessageFile :: IsINSendMessageAttachment inSendMessageAttachment => inSendMessageAttachment -> IO (Id INFile)
audioMessageFile inSendMessageAttachment  =
  sendMsg inSendMessageAttachment (mkSelector "audioMessageFile") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attachmentWithAudioMessageFile:@
attachmentWithAudioMessageFileSelector :: Selector
attachmentWithAudioMessageFileSelector = mkSelector "attachmentWithAudioMessageFile:"

-- | @Selector@ for @audioMessageFile@
audioMessageFileSelector :: Selector
audioMessageFileSelector = mkSelector "audioMessageFile"

