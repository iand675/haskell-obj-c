{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A value transformer which transforms values to and from @NSData@ by archiving and unarchiving using secure coding.
--
-- Generated bindings for @NSSecureUnarchiveFromDataTransformer@.
module ObjC.Foundation.NSSecureUnarchiveFromDataTransformer
  ( NSSecureUnarchiveFromDataTransformer
  , IsNSSecureUnarchiveFromDataTransformer(..)
  , allowedTopLevelClasses
  , allowedTopLevelClassesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | The list of allowable classes which the top-level object in the archive must conform to on encoding and decoding.
--
-- Returns the result of @+transformedValueClass@ if not @Nil;@ otherwise, currently returns @NSArray,@ @NSDictionary,@ @NSSet,@ @NSString,@ @NSNumber,@ @NSDate,@ @NSData,@ @NSURL,@ @NSUUID,@ and @NSNull.@
--
-- Can be overridden by subclasses to provide an expanded or different set of allowed transformation classes.
--
-- ObjC selector: @+ allowedTopLevelClasses@
allowedTopLevelClasses :: IO (Id NSArray)
allowedTopLevelClasses  =
  do
    cls' <- getRequiredClass "NSSecureUnarchiveFromDataTransformer"
    sendClassMessage cls' allowedTopLevelClassesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allowedTopLevelClasses@
allowedTopLevelClassesSelector :: Selector '[] (Id NSArray)
allowedTopLevelClassesSelector = mkSelector "allowedTopLevelClasses"

