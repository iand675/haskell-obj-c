{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Context of a password authentication operation.
--
-- Generated bindings for @TKTokenPasswordAuthOperation@.
module ObjC.CryptoTokenKit.TKTokenPasswordAuthOperation
  ( TKTokenPasswordAuthOperation
  , IsTKTokenPasswordAuthOperation(..)
  , password
  , setPassword
  , passwordSelector
  , setPasswordSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Password, which will be filled in by the system when 'finishWithError:' is called.
--
-- ObjC selector: @- password@
password :: IsTKTokenPasswordAuthOperation tkTokenPasswordAuthOperation => tkTokenPasswordAuthOperation -> IO (Id NSString)
password tkTokenPasswordAuthOperation =
  sendMessage tkTokenPasswordAuthOperation passwordSelector

-- | Password, which will be filled in by the system when 'finishWithError:' is called.
--
-- ObjC selector: @- setPassword:@
setPassword :: (IsTKTokenPasswordAuthOperation tkTokenPasswordAuthOperation, IsNSString value) => tkTokenPasswordAuthOperation -> value -> IO ()
setPassword tkTokenPasswordAuthOperation value =
  sendMessage tkTokenPasswordAuthOperation setPasswordSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @password@
passwordSelector :: Selector '[] (Id NSString)
passwordSelector = mkSelector "password"

-- | @Selector@ for @setPassword:@
setPasswordSelector :: Selector '[Id NSString] ()
setPasswordSelector = mkSelector "setPassword:"

