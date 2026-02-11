{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CAValueFunction@.
module ObjC.QuartzCore.CAValueFunction
  ( CAValueFunction
  , IsCAValueFunction(..)
  , functionWithName
  , name
  , functionWithNameSelector
  , nameSelector


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
functionWithName :: IsNSString name => name -> IO (Id CAValueFunction)
functionWithName name =
  do
    cls' <- getRequiredClass "CAValueFunction"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "functionWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- name@
name :: IsCAValueFunction caValueFunction => caValueFunction -> IO (Id NSString)
name caValueFunction  =
  sendMsg caValueFunction (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @functionWithName:@
functionWithNameSelector :: Selector
functionWithNameSelector = mkSelector "functionWithName:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

