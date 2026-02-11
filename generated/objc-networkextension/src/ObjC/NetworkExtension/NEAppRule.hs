{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEAppRule
--
-- The NEAppRule class declares the programmatic interface for an object that contains the match conditions for a rule that is used to match network traffic originated by applications.
--
-- NEAppRule is used in the context of a Network Extension configuration to specify what traffic should be made available to the Network Extension.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEAppRule@.
module ObjC.NetworkExtension.NEAppRule
  ( NEAppRule
  , IsNEAppRule(..)
  , initWithSigningIdentifier
  , initWithSigningIdentifier_designatedRequirement
  , matchSigningIdentifier
  , matchPath
  , setMatchPath
  , matchDomains
  , setMatchDomains
  , initWithSigningIdentifierSelector
  , initWithSigningIdentifier_designatedRequirementSelector
  , matchSigningIdentifierSelector
  , matchPathSelector
  , setMatchPathSelector
  , matchDomainsSelector
  , setMatchDomainsSelector


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

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithSigningIdentifier:
--
-- Initializes a newly-allocated NEAppRule object.
--
-- @signingIdentifier@ — The signing identifier of the executable that matches the rule.
--
-- ObjC selector: @- initWithSigningIdentifier:@
initWithSigningIdentifier :: (IsNEAppRule neAppRule, IsNSString signingIdentifier) => neAppRule -> signingIdentifier -> IO (Id NEAppRule)
initWithSigningIdentifier neAppRule  signingIdentifier =
withObjCPtr signingIdentifier $ \raw_signingIdentifier ->
    sendMsg neAppRule (mkSelector "initWithSigningIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_signingIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | initWithSigningIdentifier:designatedRequirement:
--
-- Initializes a newly-allocated NEAppRule object.
--
-- @signingIdentifier@ — The signing identifier of the executable that matches the rule.
--
-- @designatedRequirement@ — The designated requirement of the executable that matches the rule.
--
-- ObjC selector: @- initWithSigningIdentifier:designatedRequirement:@
initWithSigningIdentifier_designatedRequirement :: (IsNEAppRule neAppRule, IsNSString signingIdentifier, IsNSString designatedRequirement) => neAppRule -> signingIdentifier -> designatedRequirement -> IO (Id NEAppRule)
initWithSigningIdentifier_designatedRequirement neAppRule  signingIdentifier designatedRequirement =
withObjCPtr signingIdentifier $ \raw_signingIdentifier ->
  withObjCPtr designatedRequirement $ \raw_designatedRequirement ->
      sendMsg neAppRule (mkSelector "initWithSigningIdentifier:designatedRequirement:") (retPtr retVoid) [argPtr (castPtr raw_signingIdentifier :: Ptr ()), argPtr (castPtr raw_designatedRequirement :: Ptr ())] >>= ownedObject . castPtr

-- | matchSigningIdentifier
--
-- A string containing a signing identifier. If the code signature of the executable being evaluated has a signing identifier equal to this string and all other conditions of the rule match, then the rule matches.
--
-- ObjC selector: @- matchSigningIdentifier@
matchSigningIdentifier :: IsNEAppRule neAppRule => neAppRule -> IO (Id NSString)
matchSigningIdentifier neAppRule  =
  sendMsg neAppRule (mkSelector "matchSigningIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | matchPath
--
-- A string containing a file system path. If the file system path of the executable being evaluated is equal to this string and all other conditions of the rule match, then the rule matches. This property is optional.
--
-- ObjC selector: @- matchPath@
matchPath :: IsNEAppRule neAppRule => neAppRule -> IO (Id NSString)
matchPath neAppRule  =
  sendMsg neAppRule (mkSelector "matchPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | matchPath
--
-- A string containing a file system path. If the file system path of the executable being evaluated is equal to this string and all other conditions of the rule match, then the rule matches. This property is optional.
--
-- ObjC selector: @- setMatchPath:@
setMatchPath :: (IsNEAppRule neAppRule, IsNSString value) => neAppRule -> value -> IO ()
setMatchPath neAppRule  value =
withObjCPtr value $ \raw_value ->
    sendMsg neAppRule (mkSelector "setMatchPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | matchDomains
--
-- An array of strings. This property is actually read-only. If the destination host of the network traffic being evaluated has a suffix equal to one of the strings in this array and all other conditions of the rule match, then the rule matches. This property is optional.
--
-- ObjC selector: @- matchDomains@
matchDomains :: IsNEAppRule neAppRule => neAppRule -> IO (Id NSArray)
matchDomains neAppRule  =
  sendMsg neAppRule (mkSelector "matchDomains") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | matchDomains
--
-- An array of strings. This property is actually read-only. If the destination host of the network traffic being evaluated has a suffix equal to one of the strings in this array and all other conditions of the rule match, then the rule matches. This property is optional.
--
-- ObjC selector: @- setMatchDomains:@
setMatchDomains :: (IsNEAppRule neAppRule, IsNSArray value) => neAppRule -> value -> IO ()
setMatchDomains neAppRule  value =
withObjCPtr value $ \raw_value ->
    sendMsg neAppRule (mkSelector "setMatchDomains:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSigningIdentifier:@
initWithSigningIdentifierSelector :: Selector
initWithSigningIdentifierSelector = mkSelector "initWithSigningIdentifier:"

-- | @Selector@ for @initWithSigningIdentifier:designatedRequirement:@
initWithSigningIdentifier_designatedRequirementSelector :: Selector
initWithSigningIdentifier_designatedRequirementSelector = mkSelector "initWithSigningIdentifier:designatedRequirement:"

-- | @Selector@ for @matchSigningIdentifier@
matchSigningIdentifierSelector :: Selector
matchSigningIdentifierSelector = mkSelector "matchSigningIdentifier"

-- | @Selector@ for @matchPath@
matchPathSelector :: Selector
matchPathSelector = mkSelector "matchPath"

-- | @Selector@ for @setMatchPath:@
setMatchPathSelector :: Selector
setMatchPathSelector = mkSelector "setMatchPath:"

-- | @Selector@ for @matchDomains@
matchDomainsSelector :: Selector
matchDomainsSelector = mkSelector "matchDomains"

-- | @Selector@ for @setMatchDomains:@
setMatchDomainsSelector :: Selector
setMatchDomainsSelector = mkSelector "setMatchDomains:"

