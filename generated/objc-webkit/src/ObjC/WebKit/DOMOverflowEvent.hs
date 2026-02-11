{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMOverflowEvent@.
module ObjC.WebKit.DOMOverflowEvent
  ( DOMOverflowEvent
  , IsDOMOverflowEvent(..)
  , initOverflowEvent_horizontalOverflow_verticalOverflow
  , orient
  , horizontalOverflow
  , verticalOverflow
  , initOverflowEvent_horizontalOverflow_verticalOverflowSelector
  , orientSelector
  , horizontalOverflowSelector
  , verticalOverflowSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initOverflowEvent:horizontalOverflow:verticalOverflow:@
initOverflowEvent_horizontalOverflow_verticalOverflow :: IsDOMOverflowEvent domOverflowEvent => domOverflowEvent -> CUShort -> Bool -> Bool -> IO ()
initOverflowEvent_horizontalOverflow_verticalOverflow domOverflowEvent  orient horizontalOverflow verticalOverflow =
  sendMsg domOverflowEvent (mkSelector "initOverflowEvent:horizontalOverflow:verticalOverflow:") retVoid [argCUInt (fromIntegral orient), argCULong (if horizontalOverflow then 1 else 0), argCULong (if verticalOverflow then 1 else 0)]

-- | @- orient@
orient :: IsDOMOverflowEvent domOverflowEvent => domOverflowEvent -> IO CUShort
orient domOverflowEvent  =
  fmap fromIntegral $ sendMsg domOverflowEvent (mkSelector "orient") retCUInt []

-- | @- horizontalOverflow@
horizontalOverflow :: IsDOMOverflowEvent domOverflowEvent => domOverflowEvent -> IO Bool
horizontalOverflow domOverflowEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domOverflowEvent (mkSelector "horizontalOverflow") retCULong []

-- | @- verticalOverflow@
verticalOverflow :: IsDOMOverflowEvent domOverflowEvent => domOverflowEvent -> IO Bool
verticalOverflow domOverflowEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domOverflowEvent (mkSelector "verticalOverflow") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initOverflowEvent:horizontalOverflow:verticalOverflow:@
initOverflowEvent_horizontalOverflow_verticalOverflowSelector :: Selector
initOverflowEvent_horizontalOverflow_verticalOverflowSelector = mkSelector "initOverflowEvent:horizontalOverflow:verticalOverflow:"

-- | @Selector@ for @orient@
orientSelector :: Selector
orientSelector = mkSelector "orient"

-- | @Selector@ for @horizontalOverflow@
horizontalOverflowSelector :: Selector
horizontalOverflowSelector = mkSelector "horizontalOverflow"

-- | @Selector@ for @verticalOverflow@
verticalOverflowSelector :: Selector
verticalOverflowSelector = mkSelector "verticalOverflow"

