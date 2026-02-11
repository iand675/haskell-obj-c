{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMusicPlayerApplicationController@.
module ObjC.MediaPlayer.MPMusicPlayerApplicationController
  ( MPMusicPlayerApplicationController
  , IsMPMusicPlayerApplicationController(..)
  , performQueueTransaction_completionHandler
  , performQueueTransaction_completionHandlerSelector


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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- performQueueTransaction:completionHandler:@
performQueueTransaction_completionHandler :: IsMPMusicPlayerApplicationController mpMusicPlayerApplicationController => mpMusicPlayerApplicationController -> Ptr () -> Ptr () -> IO ()
performQueueTransaction_completionHandler mpMusicPlayerApplicationController  queueTransaction completionHandler =
  sendMsg mpMusicPlayerApplicationController (mkSelector "performQueueTransaction:completionHandler:") retVoid [argPtr (castPtr queueTransaction :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @performQueueTransaction:completionHandler:@
performQueueTransaction_completionHandlerSelector :: Selector
performQueueTransaction_completionHandlerSelector = mkSelector "performQueueTransaction:completionHandler:"

