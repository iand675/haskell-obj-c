{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ functionWithName:@
functionWithName :: IsNSString name => name -> IO (Id CAValueFunction)
functionWithName name =
  do
    cls' <- getRequiredClass "CAValueFunction"
    sendClassMessage cls' functionWithNameSelector (toNSString name)

-- | @- name@
name :: IsCAValueFunction caValueFunction => caValueFunction -> IO (Id NSString)
name caValueFunction =
  sendMessage caValueFunction nameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @functionWithName:@
functionWithNameSelector :: Selector '[Id NSString] (Id CAValueFunction)
functionWithNameSelector = mkSelector "functionWithName:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

