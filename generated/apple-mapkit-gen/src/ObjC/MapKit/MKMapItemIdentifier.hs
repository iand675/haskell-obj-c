{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapItemIdentifier@.
module ObjC.MapKit.MKMapItemIdentifier
  ( MKMapItemIdentifier
  , IsMKMapItemIdentifier(..)
  , init_
  , new
  , initWithIdentifierString
  , identifierString
  , initSelector
  , newSelector
  , initWithIdentifierStringSelector
  , identifierStringSelector


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

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMKMapItemIdentifier mkMapItemIdentifier => mkMapItemIdentifier -> IO (Id MKMapItemIdentifier)
init_ mkMapItemIdentifier  =
    sendMsg mkMapItemIdentifier (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MKMapItemIdentifier)
new  =
  do
    cls' <- getRequiredClass "MKMapItemIdentifier"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithIdentifierString:@
initWithIdentifierString :: (IsMKMapItemIdentifier mkMapItemIdentifier, IsNSString string) => mkMapItemIdentifier -> string -> IO (Id MKMapItemIdentifier)
initWithIdentifierString mkMapItemIdentifier  string =
  withObjCPtr string $ \raw_string ->
      sendMsg mkMapItemIdentifier (mkSelector "initWithIdentifierString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- identifierString@
identifierString :: IsMKMapItemIdentifier mkMapItemIdentifier => mkMapItemIdentifier -> IO (Id NSString)
identifierString mkMapItemIdentifier  =
    sendMsg mkMapItemIdentifier (mkSelector "identifierString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithIdentifierString:@
initWithIdentifierStringSelector :: Selector
initWithIdentifierStringSelector = mkSelector "initWithIdentifierString:"

-- | @Selector@ for @identifierString@
identifierStringSelector :: Selector
identifierStringSelector = mkSelector "identifierString"

