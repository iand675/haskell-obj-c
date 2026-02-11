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
  , valueWithNumberSelector
  , valueWithCategorySelector
  , initSelector
  , newSelector
  , numberSelector
  , setNumberSelector


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

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ valueWithNumber:@
valueWithNumber :: CDouble -> IO (Id AXDataPointValue)
valueWithNumber number =
  do
    cls' <- getRequiredClass "AXDataPointValue"
    sendClassMsg cls' (mkSelector "valueWithNumber:") (retPtr retVoid) [argCDouble (fromIntegral number)] >>= retainedObject . castPtr

-- | @+ valueWithCategory:@
valueWithCategory :: IsNSString category => category -> IO (Id AXDataPointValue)
valueWithCategory category =
  do
    cls' <- getRequiredClass "AXDataPointValue"
    withObjCPtr category $ \raw_category ->
      sendClassMsg cls' (mkSelector "valueWithCategory:") (retPtr retVoid) [argPtr (castPtr raw_category :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsAXDataPointValue axDataPointValue => axDataPointValue -> IO (Id AXDataPointValue)
init_ axDataPointValue  =
  sendMsg axDataPointValue (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AXDataPointValue)
new  =
  do
    cls' <- getRequiredClass "AXDataPointValue"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- number@
number :: IsAXDataPointValue axDataPointValue => axDataPointValue -> IO CDouble
number axDataPointValue  =
  sendMsg axDataPointValue (mkSelector "number") retCDouble []

-- | @- setNumber:@
setNumber :: IsAXDataPointValue axDataPointValue => axDataPointValue -> CDouble -> IO ()
setNumber axDataPointValue  value =
  sendMsg axDataPointValue (mkSelector "setNumber:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueWithNumber:@
valueWithNumberSelector :: Selector
valueWithNumberSelector = mkSelector "valueWithNumber:"

-- | @Selector@ for @valueWithCategory:@
valueWithCategorySelector :: Selector
valueWithCategorySelector = mkSelector "valueWithCategory:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @number@
numberSelector :: Selector
numberSelector = mkSelector "number"

-- | @Selector@ for @setNumber:@
setNumberSelector :: Selector
setNumberSelector = mkSelector "setNumber:"

