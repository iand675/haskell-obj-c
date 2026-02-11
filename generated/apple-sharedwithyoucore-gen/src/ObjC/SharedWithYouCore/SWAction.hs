{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SWAction@.
module ObjC.SharedWithYouCore.SWAction
  ( SWAction
  , IsSWAction(..)
  , fulfill
  , fail_
  , uuid
  , complete
  , fulfillSelector
  , failSelector
  , uuidSelector
  , completeSelector


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

import ObjC.SharedWithYouCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- fulfill@
fulfill :: IsSWAction swAction => swAction -> IO ()
fulfill swAction  =
    sendMsg swAction (mkSelector "fulfill") retVoid []

-- | @- fail@
fail_ :: IsSWAction swAction => swAction -> IO ()
fail_ swAction  =
    sendMsg swAction (mkSelector "fail") retVoid []

-- | @- uuid@
uuid :: IsSWAction swAction => swAction -> IO (Id NSUUID)
uuid swAction  =
    sendMsg swAction (mkSelector "uuid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- complete@
complete :: IsSWAction swAction => swAction -> IO Bool
complete swAction  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg swAction (mkSelector "complete") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fulfill@
fulfillSelector :: Selector
fulfillSelector = mkSelector "fulfill"

-- | @Selector@ for @fail@
failSelector :: Selector
failSelector = mkSelector "fail"

-- | @Selector@ for @uuid@
uuidSelector :: Selector
uuidSelector = mkSelector "uuid"

-- | @Selector@ for @complete@
completeSelector :: Selector
completeSelector = mkSelector "complete"

