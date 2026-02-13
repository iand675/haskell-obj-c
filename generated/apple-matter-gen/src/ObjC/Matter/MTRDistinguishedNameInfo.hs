{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents the Matter-specific components of an X.509 Distinguished Name.
--
-- Generated bindings for @MTRDistinguishedNameInfo@.
module ObjC.Matter.MTRDistinguishedNameInfo
  ( MTRDistinguishedNameInfo
  , IsMTRDistinguishedNameInfo(..)
  , new
  , init_
  , nodeID
  , fabricID
  , rootCACertificateID
  , intermediateCACertificateID
  , caseAuthenticatedTags
  , caseAuthenticatedTagsSelector
  , fabricIDSelector
  , initSelector
  , intermediateCACertificateIDSelector
  , newSelector
  , nodeIDSelector
  , rootCACertificateIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MTRDistinguishedNameInfo)
new  =
  do
    cls' <- getRequiredClass "MTRDistinguishedNameInfo"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMTRDistinguishedNameInfo mtrDistinguishedNameInfo => mtrDistinguishedNameInfo -> IO (Id MTRDistinguishedNameInfo)
init_ mtrDistinguishedNameInfo =
  sendOwnedMessage mtrDistinguishedNameInfo initSelector

-- | The Node ID contained in the DN, if any.  Will be non-nil for the subject of a valid node operational certificate.
--
-- ObjC selector: @- nodeID@
nodeID :: IsMTRDistinguishedNameInfo mtrDistinguishedNameInfo => mtrDistinguishedNameInfo -> IO (Id NSNumber)
nodeID mtrDistinguishedNameInfo =
  sendMessage mtrDistinguishedNameInfo nodeIDSelector

-- | The Fabric ID contained in the DN, if any.  Will be non-nil for the subject of a valid node operational certificate, and may be non-nil for the subject of a valid intermediate or root certificate.
--
-- ObjC selector: @- fabricID@
fabricID :: IsMTRDistinguishedNameInfo mtrDistinguishedNameInfo => mtrDistinguishedNameInfo -> IO (Id NSNumber)
fabricID mtrDistinguishedNameInfo =
  sendMessage mtrDistinguishedNameInfo fabricIDSelector

-- | The @RCAC@ ID contained in the DN, if any.  Will be non-nil for the subject of a valid root certificate.
--
-- ObjC selector: @- rootCACertificateID@
rootCACertificateID :: IsMTRDistinguishedNameInfo mtrDistinguishedNameInfo => mtrDistinguishedNameInfo -> IO (Id NSNumber)
rootCACertificateID mtrDistinguishedNameInfo =
  sendMessage mtrDistinguishedNameInfo rootCACertificateIDSelector

-- | The @ICAC@ ID contained in the DN, if any.  Will be non-nil for the subject of a valid intermediate certificate.
--
-- ObjC selector: @- intermediateCACertificateID@
intermediateCACertificateID :: IsMTRDistinguishedNameInfo mtrDistinguishedNameInfo => mtrDistinguishedNameInfo -> IO (Id NSNumber)
intermediateCACertificateID mtrDistinguishedNameInfo =
  sendMessage mtrDistinguishedNameInfo intermediateCACertificateIDSelector

-- | The set of CASE Authenticated Tags contained in the DN.  Maybe be non-empty for the subject of a valid node operational certificate.
--
-- ObjC selector: @- caseAuthenticatedTags@
caseAuthenticatedTags :: IsMTRDistinguishedNameInfo mtrDistinguishedNameInfo => mtrDistinguishedNameInfo -> IO (Id NSSet)
caseAuthenticatedTags mtrDistinguishedNameInfo =
  sendMessage mtrDistinguishedNameInfo caseAuthenticatedTagsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRDistinguishedNameInfo)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRDistinguishedNameInfo)
initSelector = mkSelector "init"

-- | @Selector@ for @nodeID@
nodeIDSelector :: Selector '[] (Id NSNumber)
nodeIDSelector = mkSelector "nodeID"

-- | @Selector@ for @fabricID@
fabricIDSelector :: Selector '[] (Id NSNumber)
fabricIDSelector = mkSelector "fabricID"

-- | @Selector@ for @rootCACertificateID@
rootCACertificateIDSelector :: Selector '[] (Id NSNumber)
rootCACertificateIDSelector = mkSelector "rootCACertificateID"

-- | @Selector@ for @intermediateCACertificateID@
intermediateCACertificateIDSelector :: Selector '[] (Id NSNumber)
intermediateCACertificateIDSelector = mkSelector "intermediateCACertificateID"

-- | @Selector@ for @caseAuthenticatedTags@
caseAuthenticatedTagsSelector :: Selector '[] (Id NSSet)
caseAuthenticatedTagsSelector = mkSelector "caseAuthenticatedTags"

