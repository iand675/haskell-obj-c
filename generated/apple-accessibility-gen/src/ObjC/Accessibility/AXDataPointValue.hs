{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Describes a single data value, either numeric or categorical. Only the @number@ property will be used for data points in a numeric axis, and only the @category@ property will be used for data points in a categorical axis.
--
-- Generated bindings for @AXDataPointValue@.
module ObjC.Accessibility.AXDataPointValue
  ( AXDataPointValue
  , IsAXDataPointValue(..)
  , valueWithNumber
  , valueWithCategory
  , init_
  , new
  , number
  , setNumber
  , category
  , setCategory
  , categorySelector
  , initSelector
  , newSelector
  , numberSelector
  , setCategorySelector
  , setNumberSelector
  , valueWithCategorySelector
  , valueWithNumberSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ valueWithNumber:@
valueWithNumber :: CDouble -> IO (Id AXDataPointValue)
valueWithNumber number =
  do
    cls' <- getRequiredClass "AXDataPointValue"
    sendClassMessage cls' valueWithNumberSelector number

-- | @+ valueWithCategory:@
valueWithCategory :: IsNSString category => category -> IO (Id AXDataPointValue)
valueWithCategory category =
  do
    cls' <- getRequiredClass "AXDataPointValue"
    sendClassMessage cls' valueWithCategorySelector (toNSString category)

-- | @- init@
init_ :: IsAXDataPointValue axDataPointValue => axDataPointValue -> IO (Id AXDataPointValue)
init_ axDataPointValue =
  sendOwnedMessage axDataPointValue initSelector

-- | @+ new@
new :: IO (Id AXDataPointValue)
new  =
  do
    cls' <- getRequiredClass "AXDataPointValue"
    sendOwnedClassMessage cls' newSelector

-- | @- number@
number :: IsAXDataPointValue axDataPointValue => axDataPointValue -> IO CDouble
number axDataPointValue =
  sendMessage axDataPointValue numberSelector

-- | @- setNumber:@
setNumber :: IsAXDataPointValue axDataPointValue => axDataPointValue -> CDouble -> IO ()
setNumber axDataPointValue value =
  sendMessage axDataPointValue setNumberSelector value

-- | @- category@
category :: IsAXDataPointValue axDataPointValue => axDataPointValue -> IO (Id NSString)
category axDataPointValue =
  sendMessage axDataPointValue categorySelector

-- | @- setCategory:@
setCategory :: (IsAXDataPointValue axDataPointValue, IsNSString value) => axDataPointValue -> value -> IO ()
setCategory axDataPointValue value =
  sendMessage axDataPointValue setCategorySelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueWithNumber:@
valueWithNumberSelector :: Selector '[CDouble] (Id AXDataPointValue)
valueWithNumberSelector = mkSelector "valueWithNumber:"

-- | @Selector@ for @valueWithCategory:@
valueWithCategorySelector :: Selector '[Id NSString] (Id AXDataPointValue)
valueWithCategorySelector = mkSelector "valueWithCategory:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AXDataPointValue)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AXDataPointValue)
newSelector = mkSelector "new"

-- | @Selector@ for @number@
numberSelector :: Selector '[] CDouble
numberSelector = mkSelector "number"

-- | @Selector@ for @setNumber:@
setNumberSelector :: Selector '[CDouble] ()
setNumberSelector = mkSelector "setNumber:"

-- | @Selector@ for @category@
categorySelector :: Selector '[] (Id NSString)
categorySelector = mkSelector "category"

-- | @Selector@ for @setCategory:@
setCategorySelector :: Selector '[Id NSString] ()
setCategorySelector = mkSelector "setCategory:"

