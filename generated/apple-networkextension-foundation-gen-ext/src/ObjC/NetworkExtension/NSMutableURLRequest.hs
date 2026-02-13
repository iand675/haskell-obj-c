{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSMutableURLRequest
--
-- An NSMutableURLRequest object represents a mutable URL load    request in a manner independent of protocol and URL scheme.
--
-- This specialization of NSURLRequest is provided to aid    developers who may find it more convenient to mutate a single request    object for a series of URL loads instead of creating an immutable    NSURLRequest for each load. This programming model is supported by    the following contract stipulation between NSMutableURLRequest and     NSURLConnection: NSURLConnection makes a deep copy of each     NSMutableURLRequest object passed to one of its initializers.        NSMutableURLRequest is designed to be extended to support    protocol-specific data by adding categories to access a property    object provided in an interface targeted at protocol implementors.        Protocol implementors should direct their attention to the    NSMutableURLRequestExtensibility category on    NSMutableURLRequest for more information on how to provide    extensions on NSMutableURLRequest to support protocol-specific    request information.    Clients of this API who wish to create NSMutableURLRequest    objects to load URL content should consult the protocol-specific    NSMutableURLRequest categories that are available. The    NSMutableHTTPURLRequest category on NSMutableURLRequest is an    example.
--
-- Generated bindings for @NSMutableURLRequest@.
module ObjC.NetworkExtension.NSMutableURLRequest
  ( NSMutableURLRequest
  , IsNSMutableURLRequest(..)
  , bindToHotspotHelperCommand
  , bindToHotspotHelperCommandSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | bindToHotspotHelperCommand:
--
-- Binds the NSMutableURLRequest to the network interface associated with   the NEHotspotHelperCommand object.
--
-- ObjC selector: @- bindToHotspotHelperCommand:@
bindToHotspotHelperCommand :: (IsNSMutableURLRequest nsMutableURLRequest, IsNEHotspotHelperCommand command) => nsMutableURLRequest -> command -> IO ()
bindToHotspotHelperCommand nsMutableURLRequest command =
  sendMessage nsMutableURLRequest bindToHotspotHelperCommandSelector (toNEHotspotHelperCommand command)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bindToHotspotHelperCommand:@
bindToHotspotHelperCommandSelector :: Selector '[Id NEHotspotHelperCommand] ()
bindToHotspotHelperCommandSelector = mkSelector "bindToHotspotHelperCommand:"

