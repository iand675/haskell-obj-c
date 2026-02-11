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
  , functionWithNameSelector
  , functionWithControlPointsSelector
  , initWithControlPointsSelector
  , getControlPointAtIndex_valuesSelector


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

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ functionWithName:@
functionWithName :: IsNSString name => name -> IO (Id CAMediaTimingFunction)
functionWithName name =
  do
    cls' <- getRequiredClass "CAMediaTimingFunction"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "functionWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @+ functionWithControlPoints::::@
functionWithControlPoints :: CFloat -> CFloat -> CFloat -> CFloat -> IO (Id CAMediaTimingFunction)
functionWithControlPoints c1x c1y c2x c2y =
  do
    cls' <- getRequiredClass "CAMediaTimingFunction"
    sendClassMsg cls' (mkSelector "functionWithControlPoints::::") (retPtr retVoid) [argCFloat (fromIntegral c1x), argCFloat (fromIntegral c1y), argCFloat (fromIntegral c2x), argCFloat (fromIntegral c2y)] >>= retainedObject . castPtr

-- | @- initWithControlPoints::::@
initWithControlPoints :: IsCAMediaTimingFunction caMediaTimingFunction => caMediaTimingFunction -> CFloat -> CFloat -> CFloat -> CFloat -> IO (Id CAMediaTimingFunction)
initWithControlPoints caMediaTimingFunction  c1x c1y c2x c2y =
  sendMsg caMediaTimingFunction (mkSelector "initWithControlPoints::::") (retPtr retVoid) [argCFloat (fromIntegral c1x), argCFloat (fromIntegral c1y), argCFloat (fromIntegral c2x), argCFloat (fromIntegral c2y)] >>= ownedObject . castPtr

-- | @- getControlPointAtIndex:values:@
getControlPointAtIndex_values :: IsCAMediaTimingFunction caMediaTimingFunction => caMediaTimingFunction -> CULong -> Ptr CFloat -> IO ()
getControlPointAtIndex_values caMediaTimingFunction  idx ptr =
  sendMsg caMediaTimingFunction (mkSelector "getControlPointAtIndex:values:") retVoid [argCULong (fromIntegral idx), argPtr ptr]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @functionWithName:@
functionWithNameSelector :: Selector
functionWithNameSelector = mkSelector "functionWithName:"

-- | @Selector@ for @functionWithControlPoints::::@
functionWithControlPointsSelector :: Selector
functionWithControlPointsSelector = mkSelector "functionWithControlPoints::::"

-- | @Selector@ for @initWithControlPoints::::@
initWithControlPointsSelector :: Selector
initWithControlPointsSelector = mkSelector "initWithControlPoints::::"

-- | @Selector@ for @getControlPointAtIndex:values:@
getControlPointAtIndex_valuesSelector :: Selector
getControlPointAtIndex_valuesSelector = mkSelector "getControlPointAtIndex:values:"

