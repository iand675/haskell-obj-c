{-# LANGUAGE DataKinds #-}
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
  , horizontalOverflowSelector
  , initOverflowEvent_horizontalOverflow_verticalOverflowSelector
  , orientSelector
  , verticalOverflowSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initOverflowEvent:horizontalOverflow:verticalOverflow:@
initOverflowEvent_horizontalOverflow_verticalOverflow :: IsDOMOverflowEvent domOverflowEvent => domOverflowEvent -> CUShort -> Bool -> Bool -> IO ()
initOverflowEvent_horizontalOverflow_verticalOverflow domOverflowEvent orient horizontalOverflow verticalOverflow =
  sendOwnedMessage domOverflowEvent initOverflowEvent_horizontalOverflow_verticalOverflowSelector orient horizontalOverflow verticalOverflow

-- | @- orient@
orient :: IsDOMOverflowEvent domOverflowEvent => domOverflowEvent -> IO CUShort
orient domOverflowEvent =
  sendMessage domOverflowEvent orientSelector

-- | @- horizontalOverflow@
horizontalOverflow :: IsDOMOverflowEvent domOverflowEvent => domOverflowEvent -> IO Bool
horizontalOverflow domOverflowEvent =
  sendMessage domOverflowEvent horizontalOverflowSelector

-- | @- verticalOverflow@
verticalOverflow :: IsDOMOverflowEvent domOverflowEvent => domOverflowEvent -> IO Bool
verticalOverflow domOverflowEvent =
  sendMessage domOverflowEvent verticalOverflowSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initOverflowEvent:horizontalOverflow:verticalOverflow:@
initOverflowEvent_horizontalOverflow_verticalOverflowSelector :: Selector '[CUShort, Bool, Bool] ()
initOverflowEvent_horizontalOverflow_verticalOverflowSelector = mkSelector "initOverflowEvent:horizontalOverflow:verticalOverflow:"

-- | @Selector@ for @orient@
orientSelector :: Selector '[] CUShort
orientSelector = mkSelector "orient"

-- | @Selector@ for @horizontalOverflow@
horizontalOverflowSelector :: Selector '[] Bool
horizontalOverflowSelector = mkSelector "horizontalOverflow"

-- | @Selector@ for @verticalOverflow@
verticalOverflowSelector :: Selector '[] Bool
verticalOverflowSelector = mkSelector "verticalOverflow"

