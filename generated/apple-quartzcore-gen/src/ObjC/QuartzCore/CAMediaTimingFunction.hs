{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CAMediaTimingFunction@.
module ObjC.QuartzCore.CAMediaTimingFunction
  ( CAMediaTimingFunction
  , IsCAMediaTimingFunction(..)
  , functionWithName
  , functionWithControlPoints
  , initWithControlPoints
  , getControlPointAtIndex_values
  , functionWithControlPointsSelector
  , functionWithNameSelector
  , getControlPointAtIndex_valuesSelector
  , initWithControlPointsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ functionWithName:@
functionWithName :: IsNSString name => name -> IO (Id CAMediaTimingFunction)
functionWithName name =
  do
    cls' <- getRequiredClass "CAMediaTimingFunction"
    sendClassMessage cls' functionWithNameSelector (toNSString name)

-- | @+ functionWithControlPoints::::@
functionWithControlPoints :: CFloat -> CFloat -> CFloat -> CFloat -> IO (Id CAMediaTimingFunction)
functionWithControlPoints c1x c1y c2x c2y =
  do
    cls' <- getRequiredClass "CAMediaTimingFunction"
    sendClassMessage cls' functionWithControlPointsSelector c1x c1y c2x c2y

-- | @- initWithControlPoints::::@
initWithControlPoints :: IsCAMediaTimingFunction caMediaTimingFunction => caMediaTimingFunction -> CFloat -> CFloat -> CFloat -> CFloat -> IO (Id CAMediaTimingFunction)
initWithControlPoints caMediaTimingFunction c1x c1y c2x c2y =
  sendOwnedMessage caMediaTimingFunction initWithControlPointsSelector c1x c1y c2x c2y

-- | @- getControlPointAtIndex:values:@
getControlPointAtIndex_values :: IsCAMediaTimingFunction caMediaTimingFunction => caMediaTimingFunction -> CULong -> Ptr CFloat -> IO ()
getControlPointAtIndex_values caMediaTimingFunction idx ptr =
  sendMessage caMediaTimingFunction getControlPointAtIndex_valuesSelector idx ptr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @functionWithName:@
functionWithNameSelector :: Selector '[Id NSString] (Id CAMediaTimingFunction)
functionWithNameSelector = mkSelector "functionWithName:"

-- | @Selector@ for @functionWithControlPoints::::@
functionWithControlPointsSelector :: Selector '[CFloat, CFloat, CFloat, CFloat] (Id CAMediaTimingFunction)
functionWithControlPointsSelector = mkSelector "functionWithControlPoints::::"

-- | @Selector@ for @initWithControlPoints::::@
initWithControlPointsSelector :: Selector '[CFloat, CFloat, CFloat, CFloat] (Id CAMediaTimingFunction)
initWithControlPointsSelector = mkSelector "initWithControlPoints::::"

-- | @Selector@ for @getControlPointAtIndex:values:@
getControlPointAtIndex_valuesSelector :: Selector '[CULong, Ptr CFloat] ()
getControlPointAtIndex_valuesSelector = mkSelector "getControlPointAtIndex:values:"

