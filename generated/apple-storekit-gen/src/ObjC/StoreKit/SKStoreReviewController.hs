{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Controller class to request a review from the current user
--
-- Generated bindings for @SKStoreReviewController@.
module ObjC.StoreKit.SKStoreReviewController
  ( SKStoreReviewController
  , IsSKStoreReviewController(..)
  , requestReview
  , requestReviewSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Request StoreKit to ask the user for an app review. Uses the keyWindow's UIWindowScene which may or may not be the scene a user is interacting with.
--
-- This may or may not show any UI.
--
-- Given this may not successfully present an alert to the user, it is not appropriate for use  from a button or any other user action. For presenting a write review form, a deep link is   available to the App Store by appending the query params "action=write-review" to a product URL.
--
-- ObjC selector: @+ requestReview@
requestReview :: IO ()
requestReview  =
  do
    cls' <- getRequiredClass "SKStoreReviewController"
    sendClassMessage cls' requestReviewSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestReview@
requestReviewSelector :: Selector '[] ()
requestReviewSelector = mkSelector "requestReview"

