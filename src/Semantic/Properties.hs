module Semantic.Properties (module Widths, module Semantic.Properties) where

import GHC.Generics

import Control.Arrow ((&&&))

import Pure.Data.JSV (JSV)
import Pure.Data.Txt (Txt)
import Pure.Data.Default (Default(..))
import Pure.View (list,mkHTML,mkSVG,View(..),Feature)

import Semantic.Utils (Width)

import Semantic.Utils as Widths
  ( pattern One, pattern Two, pattern Three, pattern Four
  , pattern Five, pattern Six, pattern Seven, pattern Eight
  , pattern Nine, pattern Ten, pattern Eleven, pattern Twelve
  , pattern Thirteen, pattern Fourteen, pattern Fifteen, pattern Sixteen
  )

import Data.Function ((&))

class HasActionProp a where
    getAction :: a -> Txt
    setAction :: Txt -> a -> a

pattern Action :: HasActionProp a => Txt -> a -> a
pattern Action t a <- (getAction &&& id -> (t,a)) where
    Action t a = setAction t a

class HasActiveProp a where
    getActive :: a -> Bool
    setActive :: Bool -> a -> a

pattern Active :: HasActiveProp a => Bool -> a -> a
pattern Active b a <- (getActive &&& id -> (b,a)) where
    Active b a = setActive b a

class HasAlignedProp a where
    getAligned :: a -> Txt
    setAligned :: Txt -> a -> a

pattern Aligned :: HasAlignedProp a => Txt -> a -> a
pattern Aligned ad a <- (getAligned &&& id -> (ad,a)) where
    Aligned ad a = setAligned ad a

class HasAnimatedProp a where
    type AnimatedProp a
    getAnimated :: a -> AnimatedProp a
    setAnimated :: AnimatedProp a -> a -> a

pattern Animated :: HasAnimatedProp a => AnimatedProp a -> a -> a
pattern Animated anim a <- (getAnimated &&& id -> (anim,a)) where
    Animated anim a = setAnimated anim a

class HasAnimationProp a where
    getAnimation :: a -> Txt
    setAnimation :: Txt -> a -> a

pattern Animation :: HasAnimationProp a => Txt -> a -> a
pattern Animation anim a <- (getAnimation &&& id -> (anim,a)) where
    Animation anim a = setAnimation anim a

data AnimationDuration
    = Uniform Int
    | Skewed
        { hide :: Int
        , show :: Int
        }
    deriving (Generic,Default,Ord,Eq)

class HasAnimationDurationProp a where
    getAnimationDuration :: a -> AnimationDuration
    setAnimationDuration :: AnimationDuration -> a -> a

pattern AnimationDuration :: HasAnimationDurationProp a => AnimationDuration -> a -> a
pattern AnimationDuration dur a <- (getAnimationDuration &&& id -> (dur,a)) where
    AnimationDuration dur a = setAnimationDuration dur a

class HasAsProp a where
    type AsProp a
    getAs :: a -> AsProp a
    setAs :: AsProp a -> a -> a

pattern As :: HasAsProp a => AsProp a -> a -> a
pattern As as a <- (getAs &&& id -> (as,a)) where
    As as a = setAs as a

instance HasAsProp (View ms) where
    type AsProp (View ms) = [Feature ms] -> [View ms] -> View ms

    -- Note: For Managed, Component, Text, Null, and Raw views, this
    -- method just extracts a reinjector that ignores its arguments.
    -- For keyed nodes, KHTML and KSVG, this method will create a
    -- `Constructor (View ms)` that adds indexes with (zip [0..]).
    getAs v =
        case v of
            HTMLView _ t _ _    -> mkHTML t
            KHTMLView _ t _ _ _ -> \fs cs -> list (mkHTML t) fs (zip [0..] cs)
            SVGView _ t _ _     -> mkSVG t
            KSVGView _ t _ _ _  -> \fs cs -> list (mkSVG t) fs (zip [0..] cs)
            v                   -> (\_ _ -> v)

    -- Note: For Managed, Coponent, Text, Null, and Raw views, this
    -- method ignores its `as` argument and returns the view as is.
    setAs as v =
        case v of
            HTMLView _ _ fs cs    -> as fs cs
            KHTMLView _ _ fs cs _ -> list as fs cs
            SVGView _ _ fs cs     -> as fs cs
            KSVGView _ _ fs cs _  -> list as fs cs
            _                     -> v

class HasAspectRatioProp a where
    getAspectRatio :: a -> Txt
    setAspectRatio :: Txt -> a -> a

pattern AspectRatio :: HasAspectRatioProp a => Txt -> a -> a
pattern AspectRatio ar a <- (getAspectRatio &&& id -> (ar,a)) where
    AspectRatio ar a = setAspectRatio ar a

class HasAttachedProp a where
    type AttachedProp a
    getAttached :: a -> AttachedProp a
    setAttached :: AttachedProp a -> a -> a

pattern Attached :: HasAttachedProp a => AttachedProp a -> a -> a
pattern Attached att a <- (getAttached &&& id -> (att,a)) where
    Attached att a = setAttached att a

class HasAttributesProp a where
    type Attribute a
    getAttributes :: a -> [Attribute a]
    setAttributes :: [Attribute a] -> a -> a

pattern Attributes :: HasAttributesProp a => [Attribute a] -> a -> a
pattern Attributes cs a <- (getAttributes &&& id -> (cs,a)) where
    Attributes cs a = setAttributes (getAttributes a ++ cs) a

infixl 1 %
(%) c as = c & Attributes as

class HasAutoHeightProp a where
    getAutoHeight :: a -> Bool
    setAutoHeight :: Bool -> a -> a

pattern AutoHeight :: HasAutoHeightProp a => a -> a
pattern AutoHeight a <- (getAutoHeight &&& id -> (True,a)) where
    AutoHeight a = setAutoHeight True a

class HasAutoplayProp a where
    getAutoplay :: a -> Bool
    setAutoplay :: Bool -> a -> a

pattern Autoplay :: HasAutoplayProp a => a -> a
pattern Autoplay a <- (getAutoplay &&& id -> (True,a)) where
    Autoplay a = setAutoplay True a

class HasAutoSuccessProp a where
    getAutoSuccess :: a -> Bool
    setAutoSuccess :: Bool -> a -> a

pattern AutoSuccess :: HasAutoSuccessProp a => a -> a
pattern AutoSuccess a <- (getAutoSuccess &&& id -> (True,a)) where
    AutoSuccess a = setAutoSuccess True a

class HasAvatarProp a where
    getAvatar :: a -> Bool
    setAvatar :: Bool -> a -> a

pattern Avatar :: HasAvatarProp a => a -> a
pattern Avatar c <- (getAvatar &&& id -> (True,c)) where
    Avatar c = setAvatar True c

class HasBasicProp a where
    type BasicProp a :: *
    type BasicProp a = Bool
    getBasic :: a -> BasicProp a
    setBasic :: BasicProp a -> a -> a

pattern Basic :: HasBasicProp a => BasicProp a -> a -> a
pattern Basic b a <- (getBasic &&& id -> (b,a)) where
    Basic b a = setBasic b a

class HasBlockProp a where
    getBlock :: a -> Bool
    setBlock :: Bool -> a -> a

pattern Block :: HasBlockProp a => a -> a
pattern Block a <- (getBlock &&& id -> (True,a)) where
    Block a = setBlock True a

class HasBlurringProp a where
    getBlurring :: a -> Bool
    setBlurring :: Bool -> a -> a

pattern Blurring :: HasBlurringProp a => a -> a
pattern Blurring a <- (getBlurring &&& id -> (True,a)) where
    Blurring a = setBlurring True a

class HasBorderedProp a where
    getBordered :: a -> Bool
    setBordered :: Bool -> a -> a

pattern Bordered :: HasBorderedProp a => a -> a
pattern Bordered a <- (getBordered &&& id -> (True,a)) where
    Bordered a = setBordered True a

class HasBorderlessProp a where
    getBorderless :: a -> Bool
    setBorderless :: Bool -> a -> a

pattern Borderless :: HasBorderlessProp a => a -> a
pattern Borderless a <- (getBorderless &&& id -> (True,a)) where
    Borderless a = setBorderless True a

class HasBottomOffsetProp a where
    getBottomOffset :: a -> Double
    setBottomOffset :: Double -> a -> a

pattern BottomOffset :: HasBottomOffsetProp a => Double -> a -> a
pattern BottomOffset mw a <- (getBottomOffset &&& id -> (mw,a)) where
    BottomOffset mw a = setBottomOffset mw a

class HasBrandedProp a where
    getBranded :: a -> Bool
    setBranded :: Bool -> a -> a

pattern Branded :: HasBrandedProp a => Bool -> a -> a
pattern Branded b a <- (getBranded &&& id -> (b,a)) where
    Branded b a = setBranded b a

class HasBulletedProp a where
    getBulleted :: a -> Bool
    setBulleted :: Bool -> a -> a

pattern Bulleted :: HasBulletedProp a => a -> a
pattern Bulleted a <- (getBulleted &&& id -> (True,a)) where
    Bulleted a = setBulleted True a

class HasCancelButtonProp a where
    type CancelButtonProp a
    getCancelButton :: a -> CancelButtonProp a
    setCancelButton :: CancelButtonProp a -> a -> a

pattern CancelButton :: HasCancelButtonProp a => CancelButtonProp a -> a -> a
pattern CancelButton cb a <- (getCancelButton &&& id -> (cb,a)) where
    CancelButton cb a = setCancelButton cb a

class HasCategoryProp a where
    getCategory :: a -> Bool
    setCategory :: Bool -> a -> a

pattern Category :: HasCategoryProp a => a -> a
pattern Category a <- (getCategory &&& id -> (True,a)) where
    Category a = setCategory True a

class HasCelledProp a where
    type CelledProp a :: *
    type CelledProp a = Bool
    getCelled :: a -> CelledProp a
    setCelled :: CelledProp a -> a -> a

pattern Celled :: HasCelledProp a => CelledProp a -> a -> a
pattern Celled c a <- (getCelled &&& id -> (c,a)) where
    Celled c a = setCelled c a

class HasCenteredProp a where
    getCentered :: a -> Bool
    setCentered :: Bool -> a -> a

pattern Centered :: HasCenteredProp a => a -> a
pattern Centered a <- (getCentered &&& id -> (True,a)) where
    Centered a = setCentered True a
class HasChildrenProp a where
    type Child a
    getChildren :: a -> [Child a]
    setChildren :: [Child a] -> a -> a

pattern Children :: HasChildrenProp a => [Child a] -> a -> a
pattern Children cs a <- (getChildren &&& id -> (cs,a)) where
    Children cs a = setChildren (getChildren a ++ cs) a

infixl 1 !
(!) c cs = Children cs c

infixl 1 |>
(|>) c cs = Children cs c

class HasCircularProp a where
    getCircular :: a -> Bool
    setCircular :: Bool -> a -> a

pattern Circular :: HasCircularProp a => a -> a
pattern Circular a <- (getCircular &&& id -> (True,a)) where
    Circular a = setCircular True a

class HasClassesProp a where
    getClasses :: a -> [Txt]
    setClasses :: [Txt] -> a -> a

pattern Classes :: HasClassesProp a => [Txt] -> a -> a
pattern Classes cs a <- (getClasses &&& id -> (cs,a)) where
    Classes cs a = setClasses (getClasses a ++ cs) a

class HasClearableProp a where
    getClearable :: a -> Maybe Txt
    setClearable :: Maybe Txt -> a -> a

pattern Clearable :: HasClearableProp a => Maybe Txt -> a -> a
pattern Clearable c a <- (getClearable &&& id -> (c,a)) where
    Clearable c a = setClearable c a

class HasClearingProp a where
    getClearing :: a -> Bool
    setClearing :: Bool -> a -> a

pattern Clearing :: HasClearingProp a => a -> a
pattern Clearing a <- (getClearing &&& id -> (True,a)) where
    Clearing a = setClearing True a

class HasCloseProp a where
    getClose :: a -> Maybe Txt
    setClose :: Maybe Txt -> a -> a

pattern Close :: HasCloseProp a => Maybe Txt -> a -> a
pattern Close c a <- (getClose &&& id -> (c,a)) where
    Close c a = setClose c a

class HasCloseOnDimmerClickProp a where
    getCloseOnDimmerClick :: a -> Bool
    setCloseOnDimmerClick :: Bool -> a -> a

pattern CloseOnDimmerClick :: HasCloseOnDimmerClickProp a => a -> a
pattern CloseOnDimmerClick a <- (getCloseOnDimmerClick &&& id -> (True,a)) where
    CloseOnDimmerClick a = setCloseOnDimmerClick True a

class HasCloseOnDocumentClickProp a where
    getCloseOnDocumentClick :: a -> Bool
    setCloseOnDocumentClick :: Bool -> a -> a

pattern CloseOnDocumentClick :: HasCloseOnDocumentClickProp a => a -> a
pattern CloseOnDocumentClick a <- (getCloseOnDocumentClick &&& id -> (True,a)) where
    CloseOnDocumentClick a = setCloseOnDocumentClick True a

pattern NoCloseOnDocumentClick :: HasCloseOnDocumentClickProp a => a -> a
pattern NoCloseOnDocumentClick a <- (getCloseOnDocumentClick &&& id -> (False,a)) where
    NoCloseOnDocumentClick a = setCloseOnDocumentClick False a

class HasCloseOnEscapeProp a where
    getCloseOnEscape :: a -> Bool
    setCloseOnEscape :: Bool -> a -> a

pattern CloseOnEscape :: HasCloseOnEscapeProp a => a -> a
pattern CloseOnEscape a <- (getCloseOnEscape &&& id -> (True,a)) where
    CloseOnEscape a = setCloseOnEscape True a

pattern NoCloseOnEscape :: HasCloseOnEscapeProp a => a -> a
pattern NoCloseOnEscape a <- (getCloseOnEscape &&& id -> (False,a)) where
    NoCloseOnEscape a = setCloseOnEscape False a

class HasCloseOnPortalMouseLeaveProp a where
    getCloseOnPortalMouseLeave :: a -> Bool
    setCloseOnPortalMouseLeave :: Bool -> a -> a

pattern CloseOnPortalMouseLeave :: HasCloseOnPortalMouseLeaveProp a => a -> a
pattern CloseOnPortalMouseLeave a <- (getCloseOnPortalMouseLeave &&& id -> (True,a)) where
    CloseOnPortalMouseLeave a = setCloseOnPortalMouseLeave True a

class HasCloseOnRootNodeClickProp a where
    getCloseOnRootNodeClick :: a -> Bool
    setCloseOnRootNodeClick :: Bool -> a -> a

pattern CloseOnRootNodeClick :: HasCloseOnRootNodeClickProp a => a -> a
pattern CloseOnRootNodeClick a <- (getCloseOnRootNodeClick &&& id -> (True,a)) where
    CloseOnRootNodeClick a = setCloseOnRootNodeClick True a

class HasCloseOnTriggerBlurProp a where
    getCloseOnTriggerBlur :: a -> Bool
    setCloseOnTriggerBlur :: Bool -> a -> a

pattern CloseOnTriggerBlur :: HasCloseOnTriggerBlurProp a => a -> a
pattern CloseOnTriggerBlur a <- (getCloseOnTriggerBlur &&& id -> (True,a)) where
    CloseOnTriggerBlur a = setCloseOnTriggerBlur True a

class HasCloseOnTriggerClickProp a where
    getCloseOnTriggerClick :: a -> Bool
    setCloseOnTriggerClick :: Bool -> a -> a

pattern CloseOnTriggerClick :: HasCloseOnTriggerClickProp a => a -> a
pattern CloseOnTriggerClick a <- (getCloseOnTriggerClick &&& id -> (True,a)) where
    CloseOnTriggerClick a = setCloseOnTriggerClick True a

class HasCloseOnTriggerMouseLeaveProp a where
    getCloseOnTriggerMouseLeave :: a -> Bool
    setCloseOnTriggerMouseLeave :: Bool -> a -> a

pattern CloseOnTriggerMouseLeave :: HasCloseOnTriggerMouseLeaveProp a => a -> a
pattern CloseOnTriggerMouseLeave a <- (getCloseOnTriggerMouseLeave &&& id -> (True,a)) where
    CloseOnTriggerMouseLeave a = setCloseOnTriggerMouseLeave True a

class HasCollapsedProp a where
    getCollapsed :: a -> Bool
    setCollapsed :: Bool -> a -> a

pattern Collapsed :: HasCollapsedProp a => Bool -> a -> a
pattern Collapsed c a <- (getCollapsed &&& id -> (c,a)) where
    Collapsed c a = setCollapsed c a

class HasCollapsingProp a where
    getCollapsing :: a -> Bool
    setCollapsing :: Bool -> a -> a

pattern Collapsing :: HasCollapsingProp a => a -> a
pattern Collapsing a <- (getCollapsing &&& id -> (True,a)) where
    Collapsing a = setCollapsing True a

class HasColorProp a where
    getColor :: a -> Txt
    setColor :: Txt -> a -> a

pattern Color :: HasColorProp a => Txt -> a -> a
pattern Color c a <- (getColor &&& id -> (c,a)) where
    Color c a = setColor c a

class HasColumnsProp a where
    getColumns :: a -> Width
    setColumns :: Width -> a -> a

pattern Columns :: HasColumnsProp a => Width -> a -> a
pattern Columns w a <- (getColumns &&& id -> (w,a)) where
    Columns w a = setColumns w a

class HasCompactProp a where
    type CompactProp a :: *
    type CompactProp a = Bool
    getCompact :: a -> CompactProp a
    setCompact :: CompactProp a -> a -> a

pattern Compact :: HasCompactProp a => CompactProp a -> a -> a
pattern Compact c a <- (getCompact &&& id -> (c,a)) where
    Compact c a = setCompact c a

class HasCompletedProp a where
    getCompleted :: a -> Bool
    setCompleted :: Bool -> a -> a

pattern Completed :: HasCompletedProp a => Bool -> a -> a
pattern Completed b a <- (getCompleted &&& id -> (b,a)) where
    Completed b a = setCompleted b a

class HasConfirmButtonProp a where
    type ConfirmButtonProp a
    getConfirmButton :: a -> ConfirmButtonProp a
    setConfirmButton :: ConfirmButtonProp a -> a -> a

pattern ConfirmButton :: HasConfirmButtonProp a => ConfirmButtonProp a -> a -> a
pattern ConfirmButton cb a <- (getConfirmButton &&& id -> (cb,a)) where
    ConfirmButton cb a = setConfirmButton cb a

class HasContextProp a where
    type ContextProp a
    getContext :: a -> ContextProp a
    setContext :: ContextProp a -> a -> a

pattern Context :: HasContextProp a => ContextProp a -> a -> a
pattern Context c a <- (getContext &&& id -> (c,a)) where
    Context c a = setContext c a

class HasContinuousProp a where
    getContinuous :: a -> Bool
    setContinuous :: Bool -> a -> a

pattern Continuous :: HasContinuousProp a => a -> a
pattern Continuous a <- (getContinuous &&& id -> (True,a)) where
    Continuous a = setContinuous True a

class HasCornerProp a where
    type CornerProp a
    getCorner :: a -> CornerProp a
    setCorner :: CornerProp a -> a -> a

pattern Corner :: HasCornerProp a => CornerProp a -> a -> a
pattern Corner c a <- (getCorner &&& id -> (c,a)) where
    Corner c a = setCorner c a

class HasCurrentRatingProp a where
    getCurrentRating :: a -> Maybe Int
    setCurrentRating :: Maybe Int -> a -> a

pattern CurrentRating :: HasCurrentRatingProp a => Maybe Int -> a -> a
pattern CurrentRating n a <- (getCurrentRating &&& id -> (n,a)) where
    CurrentRating n a = setCurrentRating n a

class HasDefaultActiveProp a where
    getDefaultActive :: a -> Bool
    setDefaultActive :: Bool -> a -> a

pattern DefaultActive :: HasDefaultActiveProp a => a -> a
pattern DefaultActive a <- (getDefaultActive &&& id -> (True,a)) where
    DefaultActive a = setDefaultActive True a

class HasDefaultOpenProp a where
    getDefaultOpen :: a -> Bool
    setDefaultOpen :: Bool -> a -> a

pattern DefaultOpen :: HasDefaultOpenProp a => a -> a
pattern DefaultOpen a <- (getDefaultOpen &&& id -> (True,a)) where
    DefaultOpen a = setDefaultOpen True a

class HasDefaultRatingProp a where
    getDefaultRating :: a -> Maybe Int
    setDefaultRating :: Maybe Int -> a -> a

pattern DefaultRating :: HasDefaultRatingProp a => Maybe Int -> a -> a
pattern DefaultRating n a <- (getDefaultRating &&& id -> (n,a)) where
    DefaultRating n a = setDefaultRating n a

class HasDefinitionProp a where
    getDefinition :: a -> Bool
    setDefinition :: Bool -> a -> a

pattern Definition :: HasDefinitionProp a => a -> a
pattern Definition a <- (getDefinition &&& id -> (True,a)) where
    Definition a = setDefinition True a

class HasDimmedProp a where
    getDimmed :: a -> Bool
    setDimmed :: Bool -> a -> a

pattern Dimmed :: HasDimmedProp a => Bool -> a -> a
pattern Dimmed b a <- (getDimmed &&& id -> (b,a)) where
    Dimmed b a = setDimmed b a

class HasDimmerTypeProp a where
    getDimmerType :: a -> Maybe Txt
    setDimmerType :: Maybe Txt -> a -> a

pattern DimmerType :: HasDimmerTypeProp a => Maybe Txt -> a -> a
pattern DimmerType b a <- (getDimmerType &&& id -> (b,a)) where
    DimmerType b a = setDimmerType b a

class HasDirectionProp a where
    getDirection :: a -> Txt
    setDirection :: Txt -> a -> a

pattern Direction :: HasDirectionProp a => Txt -> a -> a
pattern Direction d a <- (getDirection &&& id -> (d,a)) where
    Direction d a = setDirection d a

class HasDisabledProp a where
    getDisabled :: a -> Bool
    setDisabled :: Bool -> a -> a

pattern Disabled :: HasDisabledProp a => Bool -> a -> a
pattern Disabled b a <- (getDisabled &&& id -> (b,a)) where
    Disabled b a = setDisabled b a

class HasDividedProp a where
    type DividedProp a :: *
    type DividedProp a = Bool
    getDivided :: a -> DividedProp a
    setDivided :: DividedProp a -> a -> a

pattern Divided :: HasDividedProp a => DividedProp a -> a -> a
pattern Divided d a <- (getDivided &&& id -> (d,a)) where
    Divided d a = setDivided d a

class HasDividingProp a where
    getDividing :: a -> Bool
    setDividing :: Bool -> a -> a

pattern Dividing :: HasDividingProp a => a -> a
pattern Dividing a <- (getDividing &&& id -> (True,a)) where
    Dividing a = setDividing True a

class HasDoublingProp a where
    getDoubling :: a -> Bool
    setDoubling :: Bool -> a -> a

pattern Doubling :: HasDoublingProp a => a -> a
pattern Doubling a <- (getDoubling &&& id -> (True,a)) where
    Doubling a = setDoubling True a

class HasEmptyProp a where
    getEmpty :: a -> Bool
    setEmpty :: Bool -> a -> a

pattern Empty :: HasEmptyProp a => a -> a
pattern Empty a <- (getEmpty &&& id -> (True,a)) where
    Empty a = setEmpty True a

class HasErrorProp a where
    getError :: a -> Bool
    setError :: Bool -> a -> a

pattern Error :: HasErrorProp a => Bool -> a -> a
pattern Error b a <- (getError &&& id -> (b,a)) where
    Error b a = setError b a

class HasExtraProp a where
    getExtra :: a -> Bool
    setExtra :: Bool -> a -> a

pattern Extra :: HasExtraProp a => a -> a
pattern Extra a <- (getExtra &&& id -> (True,a)) where
    Extra a = setExtra True a

class HasFireOnMountProp a where
    getFireOnMount :: a -> Bool
    setFireOnMount :: Bool -> a -> a

pattern FireOnMount :: HasFireOnMountProp a => a -> a
pattern FireOnMount a <- (getFireOnMount &&& id -> (True,a)) where
    FireOnMount a = setFireOnMount True a

class HasFittedProp a where
    type FittedProp a :: *
    type FittedProp a = Bool
    getFitted :: a -> FittedProp a
    setFitted :: FittedProp a -> a -> a

pattern Fitted :: HasFittedProp a => FittedProp a -> a -> a
pattern Fitted f a <- (getFitted &&& id -> (f,a)) where
    Fitted f a = setFitted f a

class HasFixedProp a where
    type FixedProp a :: *
    type FixedProp a = Txt
    getFixed :: a -> FixedProp a
    setFixed :: FixedProp a -> a -> a

pattern Fixed :: HasFixedProp a => FixedProp a -> a -> a
pattern Fixed f a <- (getFixed &&& id -> (f,a)) where
    Fixed f a = setFixed f a

class HasFlippedProp a where
    getFlipped :: a -> Txt
    setFlipped :: Txt -> a -> a

pattern Flipped :: HasFlippedProp a => Txt -> a -> a
pattern Flipped f a <- (getFlipped &&& id -> (f,a)) where
    Flipped f a = setFlipped f a

class HasFloatedProp a where
    type FloatedProp a :: *
    type FloatedProp a = Txt
    getFloated :: a -> FloatedProp a
    setFloated :: FloatedProp a -> a -> a

pattern Floated :: HasFloatedProp a => FloatedProp a -> a -> a
pattern Floated f a <- (getFloated &&& id -> (f,a)) where
    Floated f a = setFloated f a

class HasFloatingProp a where
    getFloating :: a -> Bool
    setFloating :: Bool -> a -> a

pattern Floating :: HasFloatingProp a => a -> a
pattern Floating a <- (getFloating &&& id -> (True,a)) where
    Floating a = setFloating True a

class HasFlowingProp a where
    getFlowing :: a -> Bool
    setFlowing :: Bool -> a -> a

pattern Flowing :: HasFlowingProp a => a -> a
pattern Flowing a <- (getFlowing &&& id -> (True,a)) where
    Flowing a = setFlowing True a

class HasFluidProp a where
    getFluid :: a -> Bool
    setFluid :: Bool -> a -> a

pattern Fluid :: HasFluidProp a => a -> a
pattern Fluid a <- (getFluid &&& id -> (True,a)) where
    Fluid a = setFluid True a

class HasFocusProp a where
    getFocus :: a -> Bool
    setFocus :: Bool -> a -> a

pattern Focus :: HasFocusProp a => a -> a
pattern Focus a <- (getFocus &&& id -> (True,a)) where
    Focus a = setFocus True a

class HasFocusedProp a where
    getFocused :: a -> Bool
    setFocused :: Bool -> a -> a

pattern Focused :: HasFocusedProp a => a -> a
pattern Focused a <- (getFocused &&& id -> (True,a)) where
    Focused a = setFocused True a

class HasFullWidthProp a where
    getFullWidth :: a -> Bool
    setFullWidth :: Bool -> a -> a

pattern FullWidth :: HasFullWidthProp a => a -> a
pattern FullWidth a <- (getFullWidth &&& id -> (True,a)) where
    FullWidth a = setFullWidth True a

class HasGroupedProp a where
    getGrouped :: a -> Bool
    setGrouped :: Bool -> a -> a

pattern Grouped :: HasGroupedProp a => a -> a
pattern Grouped a <- (getGrouped &&& id -> (True,a)) where
    Grouped a = setGrouped True a

class HasHiddenProp a where
    getHidden :: a -> Bool
    setHidden :: Bool -> a -> a

pattern Hidden :: HasHiddenProp a => Bool -> a -> a
pattern Hidden b a <- (getHidden &&& id -> (b,a)) where
    Hidden b a = setHidden b a

class HasHideOnScrollProp a where
    getHideOnScroll :: a -> Bool
    setHideOnScroll :: Bool -> a -> a

pattern HideOnScroll :: HasHideOnScrollProp a => a -> a
pattern HideOnScroll a <- (getHideOnScroll &&& id -> (True,a)) where
    HideOnScroll a = setHideOnScroll True a

class HasHorizontalProp a where
    getHorizontal :: a -> Bool
    setHorizontal :: Bool -> a -> a

pattern Horizontal :: HasHorizontalProp a => a -> a
pattern Horizontal a <- (getHorizontal &&& id -> (True,a)) where
    Horizontal a = setHorizontal True a

class HasHoverableProp a where
    getHoverable :: a -> Bool
    setHoverable :: Bool -> a -> a

pattern Hoverable :: HasHoverableProp a => a -> a
pattern Hoverable a <- (getHoverable &&& id -> (True,a)) where
    Hoverable a = setHoverable True a

class HasIndexProp a where
    getIndex :: a -> Int
    setIndex :: Int -> a -> a

pattern Index :: HasIndexProp a => Int -> a -> a
pattern Index i a <- (getIndex &&& id -> (i,a)) where
    Index i a = setIndex i a

class HasIndicatingProp a where
    getIndicating :: a -> Bool
    setIndicating :: Bool -> a -> a

pattern Indicating :: HasIndicatingProp a => a -> a
pattern Indicating a <- (getIndicating &&& id -> (True,a)) where
    Indicating a = setIndicating True a

class HasInfoProp a where
    getInfo :: a -> Bool
    setInfo :: Bool -> a -> a

pattern Info :: HasInfoProp a => a -> a
pattern Info a <- (getInfo &&& id -> (True,a)) where
    Info a = setInfo True a

class HasInlineProp a where
    type InlineProp a
    getInline :: a -> InlineProp a
    setInline :: InlineProp a -> a -> a

pattern Inline :: HasInlineProp a => InlineProp a -> a -> a
pattern Inline i a <- (getInline &&& id -> (i,a)) where
    Inline i a = setInline i a

class HasInnerRefProp a where
    type InnerRefProp a
    getInnerRef :: a -> InnerRefProp a
    setInnerRef :: InnerRefProp a -> a -> a

pattern InnerRef :: HasInnerRefProp a => InnerRefProp a -> a -> a
pattern InnerRef ir a <- (getInnerRef &&& id -> (ir,a)) where
    InnerRef ir a = setInnerRef ir a

class HasInputRefProp a where
    type InputRefProp a
    getInputRef :: a -> InputRefProp a
    setInputRef :: InputRefProp a -> a -> a

pattern InputRef :: HasInputRefProp a => InputRefProp a -> a -> a
pattern InputRef ir a <- (getInputRef &&& id -> (ir,a)) where
    InputRef ir a = setInputRef ir a

class HasInstantProp a where
    getInstant :: a -> Bool
    setInstant :: Bool -> a -> a

pattern Instant :: HasInstantProp a => a -> a
pattern Instant a <- (getInstant &&& id -> (True,a)) where
    Instant a = setInstant True a

class HasInternalProp a where
    getInternal :: a -> Bool
    setInternal :: Bool -> a -> a

pattern Internal :: HasInternalProp a => a -> a
pattern Internal a <- (getInternal &&& id -> (True,a)) where
    Internal a = setInternal True a

class HasInvertedProp a where
    getInverted :: a -> Bool
    setInverted :: Bool -> a -> a

pattern Inverted :: HasInvertedProp a => a -> a
pattern Inverted a <- (getInverted &&& id -> (True,a)) where
    Inverted a = setInverted True a

class HasIsButtonProp a where
    getIsButton :: a -> Bool
    setIsButton :: Bool -> a -> a

pattern IsButton :: HasIsButtonProp a => a -> a
pattern IsButton a <- (getIsButton &&& id -> (True,a)) where
    IsButton a = setIsButton True a

data Checked = Unchecked | Indeterminate | Checked
  deriving (Eq,Ord,Enum,Bounded,Generic,Default)

class HasIsCheckedProp a where
    getIsChecked :: a -> Checked
    setIsChecked :: Checked -> a -> a

pattern IsChecked :: HasIsCheckedProp a => Checked -> a -> a
pattern IsChecked c a <- (getIsChecked &&& id -> (c,a)) where
    IsChecked c a = setIsChecked c a

class HasIsContainerProp a where
    getIsContainer :: a -> Bool
    setIsContainer :: Bool -> a -> a

pattern IsContainer :: HasIsContainerProp a => a -> a
pattern IsContainer a <- (getIsContainer &&& id -> (True,a)) where
    IsContainer a = setIsContainer True a

class HasIsHeaderProp a where
    getIsHeader :: a -> Bool
    setIsHeader :: Bool -> a -> a

pattern IsHeader :: HasIsHeaderProp a => a -> a
pattern IsHeader a <- (getIsHeader &&& id -> (True,a)) where
    IsHeader a = setIsHeader True a

class HasIsIconProp a where
    type IsIconProp a :: *
    type IsIconProp a = Maybe Txt
    getIsIcon :: a -> IsIconProp a
    setIsIcon :: IsIconProp a -> a -> a

pattern IsIcon :: HasIsIconProp a => IsIconProp a -> a -> a
pattern IsIcon i a <- (getIsIcon &&& id -> (i,a)) where
    IsIcon i a = setIsIcon i a

class HasIsImageProp a where
    getIsImage :: a -> Bool
    setIsImage :: Bool -> a -> a

pattern IsImage :: HasIsImageProp a => a -> a
pattern IsImage a <- (getIsImage &&& id -> (True,a)) where
    IsImage a = setIsImage True a

class HasIsIndeterminateProp a where
    getIsIndeterminate :: a -> Bool
    setIsIndeterminate :: Bool -> a -> a

pattern IsIndeterminate :: HasIsIndeterminateProp a => a -> a
pattern IsIndeterminate a <- (getIsIndeterminate &&& id -> (True,a)) where
    IsIndeterminate a = setIsIndeterminate True a

class HasIsItemProp a where
    getIsItem :: a -> Bool
    setIsItem :: Bool -> a -> a

pattern IsItem :: HasIsItemProp a => a -> a
pattern IsItem a <- (getIsItem &&& id -> (True,a)) where
    IsItem a = setIsItem True a

class HasIsRadioProp a where
    getIsRadio :: a -> Bool
    setIsRadio :: Bool -> a -> a

pattern IsRadio :: HasIsRadioProp a => a -> a
pattern IsRadio a <- (getIsRadio &&& id -> (True,a)) where
    IsRadio a = setIsRadio True a

class HasIsSearchProp a where
    getIsSearch :: a -> Bool
    setIsSearch :: Bool -> a -> a

pattern IsSearch :: HasIsSearchProp a => a -> a
pattern IsSearch a <- (getIsSearch &&& id -> (True,a)) where
    IsSearch a = setIsSearch True a

class HasIsTextProp a where
    getIsText :: a -> Bool
    setIsText :: Bool -> a -> a

pattern IsText :: HasIsTextProp a => a -> a
pattern IsText a <- (getIsText &&& id -> (True,a)) where
    IsText a = setIsText True a

class HasItemsPerRowProp a where
    getItemsPerRow :: a -> Width
    setItemsPerRow :: Width -> a -> a

pattern ItemsPerRow :: HasItemsPerRowProp a => Width -> a -> a
pattern ItemsPerRow w a <- (getItemsPerRow &&& id -> (w,a)) where
    ItemsPerRow w a = setItemsPerRow w a

class HasLabeledProp a where
    getLabeled :: a -> Bool
    setLabeled :: Bool -> a -> a

pattern Labeled :: HasLabeledProp a => a -> a
pattern Labeled a <- (getLabeled &&& id -> (True,a)) where
    Labeled a = setLabeled True a

class HasLabelPositionProp a where
    getLabelPosition :: a -> Txt
    setLabelPosition :: Txt -> a -> a

pattern LabelPosition :: HasLabelPositionProp a => Txt -> a -> a
pattern LabelPosition lp a <- (getLabelPosition &&& id -> (lp,a)) where
    LabelPosition lp a = setLabelPosition lp a

class HasLinkProp a where
    getLink :: a -> Bool
    setLink :: Bool -> a -> a

pattern Link :: HasLinkProp a => a -> a
pattern Link a <- (getLink &&& id -> (True,a)) where
    Link a = setLink True a

class HasLoadingProp a where
    getLoading :: a -> Bool
    setLoading :: Bool -> a -> a

pattern Loading :: HasLoadingProp a => Bool -> a -> a
pattern Loading b a <- (getLoading &&& id -> (b,a)) where
    Loading b a = setLoading b a

class HasLocalizeProp a where
    getLocalize :: a -> Txt
    setLocalize :: Txt -> a -> a

pattern Localize :: HasLocalizeProp a => Txt -> a -> a
pattern Localize l a <- (getLocalize &&& id -> (l,a)) where
    Localize l a = setLocalize l a

class HasMaxRatingProp a where
    getMaxRating :: a -> Int
    setMaxRating :: Int -> a -> a

pattern MaxRating :: HasMaxRatingProp a => Int -> a -> a
pattern MaxRating n a <- (getMaxRating &&& id -> (n,a)) where
    MaxRating n a = setMaxRating n a

class HasMaxWidthProp a where
    getMaxWidth :: a -> Int
    setMaxWidth :: Int -> a -> a

pattern MaxWidth :: HasMaxWidthProp a => Int -> a -> a
pattern MaxWidth mw a <- (getMaxWidth &&& id -> (mw,a)) where
    MaxWidth mw a = setMaxWidth mw a

class HasMinimalProp a where
    getMinimal :: a -> Bool
    setMinimal :: Bool -> a -> a

pattern Minimal :: HasMinimalProp a => a -> a
pattern Minimal a <- (getMinimal &&& id -> (True,a)) where
    Minimal a = setMinimal True a

class HasMinWidthProp a where
    getMinWidth :: a -> Int
    setMinWidth :: Int -> a -> a

pattern MinWidth :: HasMinWidthProp a => Int -> a -> a
pattern MinWidth mw a <- (getMinWidth &&& id -> (mw,a)) where
    MinWidth mw a = setMinWidth mw a

class HasMountNodeProp a where
    getMountNode :: a -> Maybe JSV
    setMountNode :: Maybe JSV -> a -> a

pattern MountNode :: HasMountNodeProp a => Maybe JSV -> a -> a
pattern MountNode mn a <- (getMountNode &&& id -> (mn,a)) where
    MountNode mn a = setMountNode mn a

class HasMountOnShowProp a where
    getMountOnShow :: a -> Bool
    setMountOnShow :: Bool -> a -> a

pattern MountOnShow :: HasMountOnShowProp a => a -> a
pattern MountOnShow a <- (getMountOnShow &&& id -> (True,a)) where
    MountOnShow a = setMountOnShow True a

class HasMouseEnterDelayProp a where
    getMouseEnterDelay :: a -> Int
    setMouseEnterDelay :: Int -> a -> a

pattern MouseEnterDelay :: HasMouseEnterDelayProp a => Int -> a -> a
pattern MouseEnterDelay d a <- (getMouseEnterDelay &&& id -> (d,a)) where
    MouseEnterDelay d a = setMouseEnterDelay d a

class HasMouseLeaveDelayProp a where
    getMouseLeaveDelay :: a -> Int
    setMouseLeaveDelay :: Int -> a -> a

pattern MouseLeaveDelay :: HasMouseLeaveDelayProp a => Int -> a -> a
pattern MouseLeaveDelay d a <- (getMouseLeaveDelay &&& id -> (d,a)) where
    MouseLeaveDelay d a = setMouseLeaveDelay d a

class HasMultipleProp a where
    getMultiple :: a -> Bool
    setMultiple :: Bool -> a -> a

pattern Multiple :: HasMultipleProp a => a -> a
pattern Multiple a <- (getMultiple &&& id -> (True,a)) where
    Multiple a = setMultiple True a

class HasNameProp a where
    getName :: a -> Txt
    setName :: Txt -> a -> a

pattern Name :: HasNameProp a => Txt -> a -> a
pattern Name n a <- (getName &&& id -> (n,a)) where
    Name n a = setName n a

class HasNegativeProp a where
    getNegative :: a -> Bool
    setNegative :: Bool -> a -> a

pattern Negative :: HasNegativeProp a => a -> a
pattern Negative a <- (getNegative &&& id -> (True,a)) where
    Negative a = setNegative True a

class HasOffsetProp a where
    type OffsetProp a
    getOffset :: a -> OffsetProp a
    setOffset :: OffsetProp a -> a -> a

pattern Offset :: HasOffsetProp a => OffsetProp a -> a -> a
pattern Offset op a <- (getOffset &&& id -> (op,a)) where
    Offset op a = setOffset op a

class HasOnBlurProp a where
    type OnBlurProp a
    getOnBlur :: a -> OnBlurProp a
    setOnBlur :: OnBlurProp a -> a -> a

pattern OnBlur :: HasOnBlurProp a => OnBlurProp a -> a -> a
pattern OnBlur ob a <- (getOnBlur &&& id -> (ob,a)) where
    OnBlur ob a = setOnBlur ob a

class HasOnBottomProp a where
    type OnBottomProp a
    getOnBottom :: a -> OnBottomProp a
    setOnBottom :: OnBottomProp a -> a -> a

pattern OnBottom :: HasOnBottomProp a => OnBottomProp a -> a -> a
pattern OnBottom obp a <- (getOnBottom &&& id -> (obp,a)) where
    OnBottom obp a = setOnBottom obp a

class HasOnBottomPassedProp a where
    type OnBottomPassedProp a
    getOnBottomPassed :: a -> OnBottomPassedProp a
    setOnBottomPassed :: OnBottomPassedProp a -> a -> a

pattern OnBottomPassed :: HasOnBottomPassedProp a => OnBottomPassedProp a -> a -> a
pattern OnBottomPassed ou a <- (getOnBottomPassed &&& id -> (ou,a)) where
    OnBottomPassed ou a = setOnBottomPassed ou a

class HasOnBottomPassedReverseProp a where
    type OnBottomPassedReverseProp a
    getOnBottomPassedReverse :: a -> OnBottomPassedReverseProp a
    setOnBottomPassedReverse :: OnBottomPassedReverseProp a -> a -> a

pattern OnBottomPassedReverse :: HasOnBottomPassedReverseProp a => OnBottomPassedReverseProp a -> a -> a
pattern OnBottomPassedReverse ou a <- (getOnBottomPassedReverse &&& id -> (ou,a)) where
    OnBottomPassedReverse ou a = setOnBottomPassedReverse ou a

class HasOnBottomVisibleProp a where
    type OnBottomVisibleProp a
    getOnBottomVisible :: a -> OnBottomVisibleProp a
    setOnBottomVisible :: OnBottomVisibleProp a -> a -> a

pattern OnBottomVisible :: HasOnBottomVisibleProp a => OnBottomVisibleProp a -> a -> a
pattern OnBottomVisible ou a <- (getOnBottomVisible &&& id -> (ou,a)) where
    OnBottomVisible ou a = setOnBottomVisible ou a

class HasOnBottomVisibleReverseProp a where
    type OnBottomVisibleReverseProp a
    getOnBottomVisibleReverse :: a -> OnBottomVisibleReverseProp a
    setOnBottomVisibleReverse :: OnBottomVisibleReverseProp a -> a -> a

pattern OnBottomVisibleReverse :: HasOnBottomVisibleReverseProp a => OnBottomVisibleReverseProp a -> a -> a
pattern OnBottomVisibleReverse ou a <- (getOnBottomVisibleReverse &&& id -> (ou,a)) where
    OnBottomVisibleReverse ou a = setOnBottomVisibleReverse ou a

class HasOnCancelProp a where
    type OnCancelProp a
    getOnCancel :: a -> OnCancelProp a
    setOnCancel :: OnCancelProp a -> a -> a

pattern OnCancel :: HasOnCancelProp a => OnCancelProp a -> a -> a
pattern OnCancel oc a <- (getOnCancel &&& id -> (oc,a)) where
    OnCancel oc a = setOnCancel oc a

class HasOnceProp a where
    getOnce :: a -> Bool
    setOnce :: Bool -> a -> a

pattern Once :: HasOnceProp a => a -> a
pattern Once a <- (getOnce &&& id -> (True,a)) where
    Once a = setOnce True a

class HasOnChangeProp a where
    type OnChangeProp a
    getOnChange :: a -> OnChangeProp a
    setOnChange :: OnChangeProp a -> a -> a

pattern OnChange :: HasOnChangeProp a => OnChangeProp a -> a -> a
pattern OnChange f a <- (getOnChange &&& id -> (f,a)) where
    OnChange f a = setOnChange f a

class HasOnClickProp a where
    type OnClickProp a
    getOnClick :: a -> OnClickProp a
    setOnClick :: OnClickProp a -> a -> a

pattern OnClick :: HasOnClickProp a => OnClickProp a -> a -> a
pattern OnClick f a <- (getOnClick &&& id -> (f,a)) where
    OnClick f a = setOnClick f a

class HasOnClickOutsideProp a where
    type OnClickOutsideProp a
    getOnClickOutside :: a -> OnClickOutsideProp a
    setOnClickOutside :: OnClickOutsideProp a -> a -> a

pattern OnClickOutside :: HasOnClickOutsideProp a => OnClickOutsideProp a -> a -> a
pattern OnClickOutside occ a <- (getOnClickOutside &&& id -> (occ,a)) where
    OnClickOutside occ a = setOnClickOutside occ a

class HasOnCloseProp a where
    type OnCloseProp a
    getOnClose :: a -> OnCloseProp a
    setOnClose :: OnCloseProp a -> a -> a

pattern OnClose :: HasOnCloseProp a => OnCloseProp a -> a -> a
pattern OnClose oc a <- (getOnClose &&& id -> (oc,a)) where
    OnClose oc a = setOnClose oc a

class HasOnCompleteProp a where
    type OnCompleteProp a
    getOnComplete :: a -> OnCompleteProp a
    setOnComplete :: OnCompleteProp a -> a -> a

pattern OnComplete :: HasOnCompleteProp a => OnCompleteProp a -> a -> a
pattern OnComplete oc a <- (getOnComplete &&& id -> (oc,a)) where
    OnComplete oc a = setOnComplete oc a

class HasOnComputerProp a where
    getOnComputer :: a -> Width
    setOnComputer :: Width -> a -> a

pattern OnComputer :: HasOnComputerProp a => Width -> a -> a
pattern OnComputer w a <- (getOnComputer &&& id -> (w,a)) where
    OnComputer w a = setOnComputer w a

class HasOnConfirmProp a where
    type OnConfirmProp a
    getOnConfirm :: a -> OnConfirmProp a
    setOnConfirm :: OnConfirmProp a -> a -> a

pattern OnConfirm :: HasOnConfirmProp a => OnConfirmProp a -> a -> a
pattern OnConfirm oc a <- (getOnConfirm &&& id -> (oc,a)) where
    OnConfirm oc a = setOnConfirm oc a

class HasOnDismissProp a where
    type OnDismissProp a
    getOnDismiss :: a -> OnDismissProp a
    setOnDismiss :: OnDismissProp a -> a -> a

pattern OnDismiss :: HasOnDismissProp a => OnDismissProp a -> a -> a
pattern OnDismiss od a <- (getOnDismiss &&& id -> (od,a)) where
    OnDismiss od a = setOnDismiss od a

class HasOnFocusProp a where
    type OnFocusProp a
    getOnFocus :: a -> OnFocusProp a
    setOnFocus :: OnFocusProp a -> a -> a

pattern OnFocus :: HasOnFocusProp a => OnFocusProp a -> a -> a
pattern OnFocus onf a <- (getOnFocus &&& id -> (onf,a)) where
    OnFocus onf a = setOnFocus onf a

class HasOnHideProp a where
    type OnHideProp a
    getOnHide :: a -> OnHideProp a
    setOnHide :: OnHideProp a -> a -> a

pattern OnHide :: HasOnHideProp a => OnHideProp a -> a -> a
pattern OnHide oh a <- (getOnHide &&& id -> (oh,a)) where
    OnHide oh a = setOnHide oh a

class HasOnInputProp a where
    type OnInputProp a
    getOnInput :: a -> OnInputProp a
    setOnInput :: OnInputProp a -> a -> a

pattern OnInput :: HasOnInputProp a => OnInputProp a -> a -> a
pattern OnInput f a <- (getOnInput &&& id -> (f,a)) where
    OnInput f a = setOnInput f a

class HasOnKeyUpProp a where
    type OnKeyUpProp a
    getOnKeyUp :: a -> OnKeyUpProp a
    setOnKeyUp :: OnKeyUpProp a -> a -> a

pattern OnKeyUp :: HasOnKeyUpProp a => OnKeyUpProp a -> a -> a
pattern OnKeyUp f a <- (getOnKeyUp &&& id -> (f,a)) where
    OnKeyUp f a = setOnKeyUp f a

class HasOnLargeScreenProp a where
    getOnLargeScreen :: a -> Width
    setOnLargeScreen :: Width -> a -> a

pattern OnLargeScreen :: HasOnLargeScreenProp a => Width -> a -> a
pattern OnLargeScreen w a <- (getOnLargeScreen &&& id -> (w,a)) where
    OnLargeScreen w a = setOnLargeScreen w a

class HasOnlyProp a where
    getOnly :: a -> [Txt]
    setOnly :: [Txt] -> a -> a

pattern Only :: HasOnlyProp a => [Txt] -> a -> a
pattern Only w a <- (getOnly &&& id -> (w,a)) where
    Only w a = setOnly w a

class HasOnMobileProp a where
    getOnMobile :: a -> Width
    setOnMobile :: Width -> a -> a

pattern OnMobile :: HasOnMobileProp a => Width -> a -> a
pattern OnMobile w a <- (getOnMobile &&& id -> (w,a)) where
    OnMobile w a = setOnMobile w a

class HasOnMountProp a where
    type OnMountProp a
    getOnMount :: a -> OnMountProp a
    setOnMount :: OnMountProp a -> a -> a

pattern OnMount :: HasOnMountProp a => OnMountProp a -> a -> a
pattern OnMount oc a <- (getOnMount &&& id -> (oc,a)) where
    OnMount oc a = setOnMount oc a

class HasOnMouseDownProp a where
    type OnMouseDownProp a
    getOnMouseDown :: a -> OnMouseDownProp a
    setOnMouseDown :: OnMouseDownProp a -> a -> a

pattern OnMouseDown :: HasOnMouseDownProp a => OnMouseDownProp a -> a -> a
pattern OnMouseDown omd a <- (getOnMouseDown &&& id -> (omd,a)) where
    OnMouseDown omd a = setOnMouseDown omd a

class HasOnMouseEnterProp a where
    type OnMouseEnterProp a
    getOnMouseEnter :: a -> OnMouseEnterProp a
    setOnMouseEnter :: OnMouseEnterProp a -> a -> a

pattern OnMouseEnter :: HasOnMouseEnterProp a => OnMouseEnterProp a -> a -> a
pattern OnMouseEnter f a <- (getOnMouseEnter &&& id -> (f,a)) where
    OnMouseEnter f a = setOnMouseEnter f a

class HasOnOffScreenProp a where
    type OnOffScreenProp a
    getOnOffScreen :: a -> OnOffScreenProp a
    setOnOffScreen :: OnOffScreenProp a -> a -> a

pattern OnOffScreen :: HasOnOffScreenProp a => OnOffScreenProp a -> a -> a
pattern OnOffScreen ou a <- (getOnOffScreen &&& id -> (ou,a)) where
    OnOffScreen ou a = setOnOffScreen ou a

class HasOnOnScreenProp a where
    type OnOnScreenProp a
    getOnOnScreen :: a -> OnOnScreenProp a
    setOnOnScreen :: OnOnScreenProp a -> a -> a

pattern OnOnScreen :: HasOnOnScreenProp a => OnOnScreenProp a -> a -> a
pattern OnOnScreen ou a <- (getOnOnScreen &&& id -> (ou,a)) where
    OnOnScreen ou a = setOnOnScreen ou a

class HasOnOpenProp a where
    type OnOpenProp a
    getOnOpen :: a -> OnOpenProp a
    setOnOpen :: OnOpenProp a -> a -> a

pattern OnOpen :: HasOnOpenProp a => OnOpenProp a -> a -> a
pattern OnOpen oc a <- (getOnOpen &&& id -> (oc,a)) where
    OnOpen oc a = setOnOpen oc a

class HasOnPassedProp a where
    type OnPassedProp a
    getOnPassed :: a -> OnPassedProp a
    setOnPassed :: OnPassedProp a -> a -> a

pattern OnPassed :: HasOnPassedProp a => OnPassedProp a -> a -> a
pattern OnPassed ou a <- (getOnPassed &&& id -> (ou,a)) where
    OnPassed ou a = setOnPassed ou a

class HasOnPassingProp a where
    type OnPassingProp a
    getOnPassing :: a -> OnPassingProp a
    setOnPassing :: OnPassingProp a -> a -> a

pattern OnPassing :: HasOnPassingProp a => OnPassingProp a -> a -> a
pattern OnPassing ou a <- (getOnPassing &&& id -> (ou,a)) where
    OnPassing ou a = setOnPassing ou a

class HasOnPassingReverseProp a where
    type OnPassingReverseProp a
    getOnPassingReverse :: a -> OnPassingReverseProp a
    setOnPassingReverse :: OnPassingReverseProp a -> a -> a

pattern OnPassingReverse :: HasOnPassingReverseProp a => OnPassingReverseProp a -> a -> a
pattern OnPassingReverse ou a <- (getOnPassingReverse &&& id -> (ou,a)) where
    OnPassingReverse ou a = setOnPassingReverse ou a

class HasOnRateProp a where
    type OnRateProp a
    getOnRate :: a -> OnRateProp a
    setOnRate :: OnRateProp a -> a -> a

pattern OnRate :: HasOnRateProp a => OnRateProp a -> a -> a
pattern OnRate or a <- (getOnRate &&& id -> (or,a)) where
    OnRate or a = setOnRate or a

class HasOnShowProp a where
    type OnShowProp a
    getOnShow :: a -> OnShowProp a
    setOnShow :: OnShowProp a -> a -> a

pattern OnShow :: HasOnShowProp a => OnShowProp a -> a -> a
pattern OnShow os a <- (getOnShow &&& id -> (os,a)) where
    OnShow os a = setOnShow os a

class HasOnStartProp a where
    type OnStartProp a
    getOnStart :: a -> OnStartProp a
    setOnStart :: OnStartProp a -> a -> a

pattern OnStart :: HasOnStartProp a => OnStartProp a -> a -> a
pattern OnStart os a <- (getOnStart &&& id -> (os,a)) where
    OnStart os a = setOnStart os a

class HasOnStickProp a where
    type OnStickProp a
    getOnStick :: a -> OnStickProp a
    setOnStick :: OnStickProp a -> a -> a

pattern OnStick :: HasOnStickProp a => OnStickProp a -> a -> a
pattern OnStick osp a <- (getOnStick &&& id -> (osp,a)) where
    OnStick osp a = setOnStick osp a

class HasOnSubmitProp a where
    type OnSubmitProp a
    getOnSubmit :: a -> OnSubmitProp a
    setOnSubmit :: OnSubmitProp a -> a -> a

pattern OnSubmit :: HasOnSubmitProp a => OnSubmitProp a -> a -> a
pattern OnSubmit f a <- (getOnSubmit &&& id -> (f,a)) where
    OnSubmit f a = setOnSubmit f a

class HasOnTabletProp a where
    getOnTablet :: a -> Width
    setOnTablet :: Width -> a -> a

pattern OnTablet :: HasOnTabletProp a => Width -> a -> a
pattern OnTablet w a <- (getOnTablet &&& id -> (w,a)) where
    OnTablet w a = setOnTablet w a

class HasOnTopProp a where
    type OnTopProp a
    getOnTop :: a -> OnTopProp a
    setOnTop :: OnTopProp a -> a -> a

pattern OnTop :: HasOnTopProp a => OnTopProp a -> a -> a
pattern OnTop ou a <- (getOnTop &&& id -> (ou,a)) where
    OnTop ou a = setOnTop ou a

class HasOnTopPassedProp a where
    type OnTopPassedProp a
    getOnTopPassed :: a -> OnTopPassedProp a
    setOnTopPassed :: OnTopPassedProp a -> a -> a

pattern OnTopPassed :: HasOnTopPassedProp a => OnTopPassedProp a -> a -> a
pattern OnTopPassed ou a <- (getOnTopPassed &&& id -> (ou,a)) where
    OnTopPassed ou a = setOnTopPassed ou a

class HasOnTopPassedReverseProp a where
    type OnTopPassedReverseProp a
    getOnTopPassedReverse :: a -> OnTopPassedReverseProp a
    setOnTopPassedReverse :: OnTopPassedReverseProp a -> a -> a

pattern OnTopPassedReverse :: HasOnTopPassedReverseProp a => OnTopPassedReverseProp a -> a -> a
pattern OnTopPassedReverse ou a <- (getOnTopPassedReverse &&& id -> (ou,a)) where
    OnTopPassedReverse ou a = setOnTopPassedReverse ou a

class HasOnTopVisibleProp a where
    type OnTopVisibleProp a
    getOnTopVisible :: a -> OnTopVisibleProp a
    setOnTopVisible :: OnTopVisibleProp a -> a -> a

pattern OnTopVisible :: HasOnTopVisibleProp a => OnTopVisibleProp a -> a -> a
pattern OnTopVisible ou a <- (getOnTopVisible &&& id -> (ou,a)) where
    OnTopVisible ou a = setOnTopVisible ou a

class HasOnTopVisibleReverseProp a where
    type OnTopVisibleReverseProp a
    getOnTopVisibleReverse :: a -> OnTopVisibleReverseProp a
    setOnTopVisibleReverse :: OnTopVisibleReverseProp a -> a -> a

pattern OnTopVisibleReverse :: HasOnTopVisibleReverseProp a => OnTopVisibleReverseProp a -> a -> a
pattern OnTopVisibleReverse ou a <- (getOnTopVisibleReverse &&& id -> (ou,a)) where
    OnTopVisibleReverse ou a = setOnTopVisibleReverse ou a

class HasOnUnmountProp a where
    type OnUnmountProp a
    getOnUnmount :: a -> OnUnmountProp a
    setOnUnmount :: OnUnmountProp a -> a -> a

pattern OnUnmount :: HasOnUnmountProp a => OnUnmountProp a -> a -> a
pattern OnUnmount oc a <- (getOnUnmount &&& id -> (oc,a)) where
    OnUnmount oc a = setOnUnmount oc a

class HasOnUnstickProp a where
    type OnUnstickProp a
    getOnUnstick :: a -> OnUnstickProp a
    setOnUnstick :: OnUnstickProp a -> a -> a

pattern OnUnstick :: HasOnUnstickProp a => OnUnstickProp a -> a -> a
pattern OnUnstick ou a <- (getOnUnstick &&& id -> (ou,a)) where
    OnUnstick ou a = setOnUnstick ou a

class HasOnUpdateProp a where
    type OnUpdateProp a
    getOnUpdate :: a -> OnUpdateProp a
    setOnUpdate :: OnUpdateProp a -> a -> a

pattern OnUpdate :: HasOnUpdateProp a => OnUpdateProp a -> a -> a
pattern OnUpdate ou a <- (getOnUpdate &&& id -> (ou,a)) where
    OnUpdate ou a = setOnUpdate ou a

class HasOnWidescreenProp a where
    getOnWidescreen :: a -> Width
    setOnWidescreen :: Width -> a -> a

pattern OnWidescreen :: HasOnWidescreenProp a => Width -> a -> a
pattern OnWidescreen w a <- (getOnWidescreen &&& id -> (w,a)) where
    OnWidescreen w a = setOnWidescreen w a

class HasOpenProp a where
    type OpenProp a :: *
    type OpenProp a = Bool
    getOpen :: a -> OpenProp a
    setOpen :: OpenProp a -> a -> a

pattern Open :: HasOpenProp a => OpenProp a -> a -> a
pattern Open o a <- (getOpen &&& id -> (o,a)) where
    Open o a = setOpen o a

class HasOpenOnTriggerClickProp a where
    getOpenOnTriggerClick :: a -> Bool
    setOpenOnTriggerClick :: Bool -> a -> a

pattern OpenOnTriggerClick :: HasOpenOnTriggerClickProp a => a -> a
pattern OpenOnTriggerClick a <- (getOpenOnTriggerClick &&& id -> (True,a)) where
    OpenOnTriggerClick a = setOpenOnTriggerClick True a

pattern NoOpenOnTriggerClick :: HasOpenOnTriggerClickProp a => a -> a
pattern NoOpenOnTriggerClick a <- (getOpenOnTriggerClick &&& id -> (False,a)) where
    NoOpenOnTriggerClick a = setOpenOnTriggerClick False a

class HasOpenOnTriggerFocusProp a where
    getOpenOnTriggerFocus :: a -> Bool
    setOpenOnTriggerFocus :: Bool -> a -> a

pattern OpenOnTriggerFocus :: HasOpenOnTriggerFocusProp a => a -> a
pattern OpenOnTriggerFocus a <- (getOpenOnTriggerFocus &&& id -> (True,a)) where
    OpenOnTriggerFocus a = setOpenOnTriggerFocus True a

pattern NoOpenOnTriggerFocus :: HasOpenOnTriggerFocusProp a => a -> a
pattern NoOpenOnTriggerFocus a <- (getOpenOnTriggerFocus &&& id -> (False,a)) where
    NoOpenOnTriggerFocus a = setOpenOnTriggerFocus False a

class HasOpenOnTriggerMouseEnterProp a where
    getOpenOnTriggerMouseEnter :: a -> Bool
    setOpenOnTriggerMouseEnter :: Bool -> a -> a

pattern OpenOnTriggerMouseEnter :: HasOpenOnTriggerMouseEnterProp a => a -> a
pattern OpenOnTriggerMouseEnter a <- (getOpenOnTriggerMouseEnter &&& id -> (True,a)) where
    OpenOnTriggerMouseEnter a = setOpenOnTriggerMouseEnter True a

pattern NoOpenOnTriggerMouseEnter :: HasOpenOnTriggerMouseEnterProp a => a -> a
pattern NoOpenOnTriggerMouseEnter a <- (getOpenOnTriggerMouseEnter &&& id -> (False,a)) where
    NoOpenOnTriggerMouseEnter a = setOpenOnTriggerMouseEnter False a

class HasOrderedProp a where
    getOrdered :: a -> Bool
    setOrdered :: Bool -> a -> a

pattern Ordered :: HasOrderedProp a => a -> a
pattern Ordered a <- (getOrdered &&& id -> (True,a)) where
    Ordered a = setOrdered True a

class HasPaddedProp a where
    getPadded :: a -> Maybe Txt
    setPadded :: Maybe Txt -> a -> a

pattern Padded :: HasPaddedProp a => Maybe Txt -> a -> a
pattern Padded p a <- (getPadded &&& id -> (p,a)) where
    Padded p a = setPadded p a

class HasPageProp a where
    getPage :: a -> Bool
    setPage :: Bool -> a -> a

pattern Page :: HasPageProp a => a -> a
pattern Page a <- (getPage &&& id -> (True,a)) where
    Page a = setPage True a

class HasPaginationProp a where
    getPagination :: a -> Bool
    setPagination :: Bool -> a -> a

pattern Pagination :: HasPaginationProp a => a -> a
pattern Pagination a <- (getPagination &&& id -> (True,a)) where
    Pagination a = setPagination True a

class HasPercentProp a where
    getPercent :: a -> Maybe Double
    setPercent :: Maybe Double -> a -> a

pattern Percent :: HasPercentProp a => Double -> a -> a
pattern Percent p a <- (getPercent &&& id -> (Just p,a)) where
    Percent p a = setPercent (Just p) a

class HasPiledProp a where
    getPiled :: a -> Bool
    setPiled :: Bool -> a -> a

pattern Piled :: HasPiledProp a => a -> a
pattern Piled a <- (getPiled &&& id -> (True,a)) where
    Piled a = setPiled True a

class HasPlaceholderProp a where
    getPlaceholder :: a -> Txt
    setPlaceholder :: Txt -> a -> a

pattern Placeholder :: HasPlaceholderProp a => Txt -> a -> a
pattern Placeholder ph a <- (getPlaceholder &&& id -> (ph,a)) where
    Placeholder ph a = setPlaceholder ph a

class HasPointingProp a where
    type PointingProp a :: *
    type PointingProp a = Maybe Txt
    getPointing :: a -> PointingProp a
    setPointing :: PointingProp a -> a -> a

pattern Pointing :: HasPointingProp a => PointingProp a -> a -> a
pattern Pointing p a <- (getPointing &&& id -> (p,a)) where
    Pointing p a = setPointing p a

class HasPositionProp a where
    getPosition :: a -> Txt
    setPosition :: Txt -> a -> a

pattern Position :: HasPositionProp a => Txt -> a -> a
pattern Position p a <- (getPosition &&& id -> (p,a)) where
    Position p a = setPosition p a

class HasPositiveProp a where
    getPositive :: a -> Bool
    setPositive :: Bool -> a -> a

pattern Positive :: HasPositiveProp a => a -> a
pattern Positive a <- (getPositive &&& id -> (True,a)) where
    Positive a = setPositive True a

class HasPrecisionProp a where
    getPrecision :: a -> Int
    setPrecision :: Int -> a -> a

pattern Precision :: HasPrecisionProp a => Int -> a -> a
pattern Precision p a <- (getPrecision &&& id -> (p,a)) where
    Precision p a = setPrecision p a

class HasPrependProp a where
    getPrepend :: a -> Bool
    setPrepend :: Bool -> a -> a

pattern Prepend :: HasPrependProp a => Bool -> a -> a
pattern Prepend o a <- (getPrepend &&& id -> (o,a)) where
    Prepend o a = setPrepend o a

class HasPrimaryProp a where
    getPrimary :: a -> Bool
    setPrimary :: Bool -> a -> a

pattern Primary :: HasPrimaryProp a => a -> a
pattern Primary a <- (getPrimary &&& id -> (True,a)) where
    Primary a = setPrimary True a

class HasPushingProp a where
    getPushing :: a -> Bool
    setPushing :: Bool -> a -> a

pattern Pushing :: HasPushingProp a => a -> a
pattern Pushing a <- (getPushing &&& id -> (True,a)) where
    Pushing a = setPushing True a

class HasRaisedProp a where
    getRaised :: a -> Bool
    setRaised :: Bool -> a -> a

pattern Raised :: HasRaisedProp a => a -> a
pattern Raised a <- (getRaised &&& id -> (True,a)) where
    Raised a = setRaised True a

class HasReadOnlyProp a where
    getReadOnly :: a -> Bool
    setReadOnly :: Bool -> a -> a

pattern ReadOnly :: HasReadOnlyProp a => a -> a
pattern ReadOnly a <- (getReadOnly &&& id -> (True,a)) where
    ReadOnly a = setReadOnly True a

class HasRefProp a where
    type RefProp a
    getRef :: a -> RefProp a
    setRef :: RefProp a -> a -> a

pattern Ref :: HasRefProp a => RefProp a -> a -> a
pattern Ref r a <- (getRef &&& id -> (r,a)) where
    Ref r a = setRef r a

class HasRelaxedProp a where
    getRelaxed :: a -> Maybe Txt
    setRelaxed :: Maybe Txt -> a -> a

pattern Relaxed :: HasRelaxedProp a => Maybe Txt -> a -> a
pattern Relaxed r a <- (getRelaxed &&& id -> (r,a)) where
    Relaxed r a = setRelaxed r a

class HasReplyProp a where
    getReply :: a -> Bool
    setReply :: Bool -> a -> a

pattern Reply :: HasReplyProp a => a -> a
pattern Reply a <- (getReply &&& id -> (True,a)) where
    Reply a = setReply True a

class HasRequiredProp a where
    getRequired :: a -> Bool
    setRequired :: Bool -> a -> a

pattern Required :: HasRequiredProp a => a -> a
pattern Required a <- (getRequired &&& id -> (True,a)) where
    Required a = setRequired True a

class HasReversedProp a where
    type ReversedProp a
    getReversed :: a -> ReversedProp a
    setReversed :: ReversedProp a -> a -> a

pattern Reversed :: HasReversedProp a => ReversedProp a -> a -> a
pattern Reversed r a <- (getReversed &&& id -> (r,a)) where
    Reversed r a = setReversed r a

class HasRibbonProp a where
    getRibbon :: a -> Maybe Txt
    setRibbon :: Maybe Txt -> a -> a

pattern Ribbon :: HasRibbonProp a => Maybe Txt -> a -> a
pattern Ribbon r a <- (getRibbon &&& id -> (r,a)) where
    Ribbon r a = setRibbon r a

class HasRotatedProp a where
    getRotated :: a -> Txt
    setRotated :: Txt -> a -> a

pattern Rotated :: HasRotatedProp a => Txt -> a -> a
pattern Rotated r a <- (getRotated &&& id -> (r,a)) where
    Rotated r a = setRotated r a

class HasRoundedProp a where
    getRounded :: a -> Bool
    setRounded :: Bool -> a -> a

pattern Rounded :: HasRoundedProp a => a -> a
pattern Rounded a <- (getRounded &&& id -> (True,a)) where
    Rounded a = setRounded True a

class HasRowsProp a where
    getRows :: a -> Int
    setRows :: Int -> a -> a

pattern Rows :: HasRowsProp a => Int -> a -> a
pattern Rows n a <- (getRows &&& id -> (n,a)) where
    Rows n a = setRows n a

class HasScrollableProp a where
    getScrollable :: a -> Bool
    setScrollable :: Bool -> a -> a

pattern Scrollable :: HasScrollableProp a => a -> a
pattern Scrollable a <- (getScrollable &&& id -> (True,a)) where
    Scrollable a = setScrollable True a

class HasScrollContextProp a where
    getScrollContext :: a -> Maybe JSV
    setScrollContext :: Maybe JSV -> a -> a

pattern ScrollContext :: HasScrollContextProp a => Maybe JSV -> a -> a
pattern ScrollContext sc a <- (getScrollContext &&& id -> (sc,a)) where
    ScrollContext sc a = setScrollContext sc a

class HasScrollingProp a where
    getScrolling :: a -> Bool
    setScrolling :: Bool -> a -> a

pattern Scrolling :: HasScrollingProp a => a -> a
pattern Scrolling a <- (getScrolling &&& id -> (True,a)) where
    Scrolling a = setScrolling True a

class HasSecondaryProp a where
    getSecondary :: a -> Bool
    setSecondary :: Bool -> a -> a

pattern Secondary :: HasSecondaryProp a => a -> a
pattern Secondary a <- (getSecondary &&& id -> (True,a)) where
    Secondary a = setSecondary True a

class HasSectionProp a where
    getSection :: a -> Bool
    setSection :: Bool -> a -> a

pattern Section :: HasSectionProp a => a -> a
pattern Section a <- (getSection &&& id -> (True,a)) where
    Section a = setSection True a

class HasSelectableProp a where
    getSelectable :: a -> Bool
    setSelectable :: Bool -> a -> a

pattern Selectable :: HasSelectableProp a => a -> a
pattern Selectable a <- (getSelectable &&& id -> (True,a)) where
    Selectable a = setSelectable True a

class HasSelectedProp a where
    getSelected :: a -> Bool
    setSelected :: Bool -> a -> a

pattern Selected :: HasSelectedProp a => Bool -> a -> a
pattern Selected b a <- (getSelected &&& id -> (b,a)) where
    Selected b a = setSelected b a

class HasSelectionProp a where
    getSelection :: a -> Bool
    setSelection :: Bool -> a -> a

pattern Selection :: HasSelectionProp a => a -> a
pattern Selection a <- (getSelection &&& id -> (True,a)) where
    Selection a = setSelection True a

class HasSimpleProp a where
    getSimple :: a -> Bool
    setSimple :: Bool -> a -> a

pattern Simple :: HasSimpleProp a => a -> a
pattern Simple a <- (getSimple &&& id -> (True,a)) where
    Simple a = setSimple True a

class HasSingleLineProp a where
    getSingleLine :: a -> Bool
    setSingleLine :: Bool -> a -> a

pattern SingleLine :: HasSingleLineProp a => a -> a
pattern SingleLine a <- (getSingleLine &&& id -> (True,a)) where
    SingleLine a = setSingleLine True a

class HasSizeProp a where
    getSize :: a -> Txt
    setSize :: Txt -> a -> a

pattern Size :: HasSizeProp a => Txt -> a -> a
pattern Size s a <- (getSize &&& id -> (s,a)) where
    Size s a = setSize s a

class HasSliderProp a where
    getSlider :: a -> Bool
    setSlider :: Bool -> a -> a

pattern Slider :: HasSliderProp a => a -> a
pattern Slider a <- (getSlider &&& id -> (True,a)) where
    Slider a = setSlider True a

class HasSortableProp a where
    getSortable :: a -> Bool
    setSortable :: Bool -> a -> a

pattern Sortable :: HasSortableProp a => a -> a
pattern Sortable a <- (getSortable &&& id -> (True,a)) where
    Sortable a = setSortable True a

class HasSortedProp a where
    getSorted :: a -> Txt
    setSorted :: Txt -> a -> a

pattern Sorted :: HasSortedProp a => Txt -> a -> a
pattern Sorted s a <- (getSorted &&& id -> (s,a)) where
    Sorted s a = setSorted s a

class HasSpacedProp a where
    getSpaced :: a -> Maybe Txt
    setSpaced :: Maybe Txt -> a -> a

pattern Spaced :: HasSpacedProp a => Maybe Txt -> a -> a
pattern Spaced s a <- (getSpaced &&& id -> (s,a)) where
    Spaced s a = setSpaced s a

class HasSrcProp a where
    getSrc :: a -> Txt
    setSrc :: Txt -> a -> a

pattern Src :: HasSrcProp a => Txt -> a -> a
pattern Src s a <- (getSrc &&& id -> (s,a)) where
    Src s a = setSrc s a

class HasStackableProp a where
    type StackableProp a :: *
    type StackableProp a = Txt
    getStackable :: a -> StackableProp a
    setStackable :: StackableProp a -> a -> a

pattern Stackable :: HasStackableProp a => StackableProp a -> a -> a
pattern Stackable s a <- (getStackable &&& id -> (s,a)) where
    Stackable s a = setStackable s a

class HasStackedProp a where
    getStacked :: a -> Bool
    setStacked :: Bool -> a -> a

pattern Stacked :: HasStackedProp a => a -> a
pattern Stacked a <- (getStacked &&& id -> (True,a)) where
    Stacked a = setStacked True a

class HasStretchedProp a where
    getStretched :: a -> Bool
    setStretched :: Bool -> a -> a

pattern Stretched :: HasStretchedProp a => a -> a
pattern Stretched a <- (getStretched &&& id -> (True,a)) where
    Stretched a = setStretched True a

class HasStripedProp a where
    getStriped :: a -> Bool
    setStriped :: Bool -> a -> a

pattern Striped :: HasStripedProp a => a -> a
pattern Striped a <- (getStriped &&& id -> (True,a)) where
    Striped a = setStriped True a

class HasStructuredProp a where
    getStructured :: a -> Bool
    setStructured :: Bool -> a -> a

pattern Structured :: HasStructuredProp a => a -> a
pattern Structured a <- (getStructured &&& id -> (True,a)) where
    Structured a = setStructured True a

class HasStyledProp a where
    getStyled :: a -> Bool
    setStyled :: Bool -> a -> a

pattern Styled :: HasStyledProp a => a -> a
pattern Styled a <- (getStyled &&& id -> (True,a)) where
    Styled a = setStyled True a

class HasStylesProp a where
    getStyles :: a -> [(Txt,Txt)]
    setStyles :: [(Txt,Txt)] -> a -> a

pattern Styles :: HasStylesProp a => [(Txt,Txt)] -> a -> a
pattern Styles ss a <- (getStyles &&& id -> (ss,a)) where
    Styles ss a = setStyles ss a

class HasSubProp a where
    getSub :: a -> Bool
    setSub :: Bool -> a -> a

pattern Sub :: HasSubProp a => a -> a
pattern Sub a <- (getSub &&& id -> (True,a)) where
    Sub a = setSub True a

class HasSuccessProp a where
    getSuccess :: a -> Bool
    setSuccess :: Bool -> a -> a

pattern Success :: HasSuccessProp a => Bool -> a -> a
pattern Success b a <- (getSuccess &&& id -> (b,a)) where
    Success b a = setSuccess b a

class HasTabIndexProp a where
    getTabIndex :: a -> Maybe Int
    setTabIndex :: Maybe Int -> a -> a

pattern TabIndex :: HasTabIndexProp a => Maybe Int -> a -> a
pattern TabIndex n a <- (getTabIndex &&& id -> (n,a)) where
    TabIndex n a = setTabIndex n a

class HasTabularProp a where
    getTabular :: a -> Maybe Txt
    setTabular :: Maybe Txt -> a -> a

pattern Tabular :: HasTabularProp a => Maybe Txt -> a -> a
pattern Tabular i a <- (getTabular &&& id -> (i,a)) where
    Tabular i a = setTabular i a

class HasTagProp a where
    getTag :: a -> Bool
    setTag :: Bool -> a -> a

pattern Tag :: HasTagProp a => a -> a
pattern Tag a <- (getTag &&& id -> (True,a)) where
    Tag a = setTag True a

class HasTertiaryProp a where
    getTertiary :: a -> Bool
    setTertiary :: Bool -> a -> a

pattern Tertiary :: HasTertiaryProp a => a -> a
pattern Tertiary a <- (getTertiary &&& id -> (True,a)) where
    Tertiary a = setTertiary True a

class HasTestProp a where
    getTest :: a -> Txt
    setTest :: Txt -> a -> a

pattern Test :: HasTestProp a => Txt -> a -> a
pattern Test t a <- (getTest &&& id -> (t,a)) where
    Test t a = setTest t a

class HasTextAlignProp a where
    getTextAlign :: a -> Txt
    setTextAlign :: Txt -> a -> a

pattern TextAlign :: HasTextAlignProp a => Txt -> a -> a
pattern TextAlign ta a <- (getTextAlign &&& id -> (ta,a)) where
    TextAlign ta a = setTextAlign ta a

class HasThreadedProp a where
    getThreaded :: a -> Bool
    setThreaded :: Bool -> a -> a

pattern Threaded :: HasThreadedProp a => a -> a
pattern Threaded a <- (getThreaded &&& id -> (True,a)) where
    Threaded a = setThreaded True a

class HasToggleProp a where
    getToggle :: a -> Bool
    setToggle :: Bool -> a -> a

pattern Toggle :: HasToggleProp a => a -> a
pattern Toggle a <- (getToggle &&& id -> (True,a)) where
    Toggle a = setToggle True a

class HasTotalProp a where
    getTotal :: a -> Int
    setTotal :: Int -> a -> a

pattern Total :: HasTotalProp a => Int -> a -> a
pattern Total p a <- (getTotal &&& id -> (p,a)) where
    Total p a = setTotal p a

class HasTransitionOnMountProp a where
    getTransitionOnMount :: a -> Bool
    setTransitionOnMount :: Bool -> a -> a

pattern TransitionOnMount :: HasTransitionOnMountProp a => a -> a
pattern TransitionOnMount a <- (getTransitionOnMount &&& id -> (True,a)) where
    TransitionOnMount a = setTransitionOnMount True a

class HasTransparentProp a where
    getTransparent :: a -> Bool
    setTransparent :: Bool -> a -> a

pattern Transparent :: HasTransparentProp a => a -> a
pattern Transparent a <- (getTransparent &&& id -> (True,a)) where
    Transparent a = setTransparent True a

class HasTriggerProp a where
    type TriggerProp a
    getTrigger :: a -> TriggerProp a
    setTrigger :: TriggerProp a -> a -> a

pattern Trigger :: HasTriggerProp a => TriggerProp a -> a -> a
pattern Trigger t a <- (getTrigger &&& id -> (t,a)) where
    Trigger t a = setTrigger t a

class HasTriggerOnProp a where
    getTriggerOn :: a -> [Txt]
    setTriggerOn :: [Txt] -> a -> a

pattern TriggerOn :: HasTriggerOnProp a => [Txt] -> a -> a
pattern TriggerOn ts a <- (getTriggerOn &&& id -> (ts,a)) where
    TriggerOn ts a = setTriggerOn ts a

class HasTypeProp a where
    getType :: a -> Txt
    setType :: Txt -> a -> a

pattern Type :: HasTypeProp a => Txt -> a -> a
pattern Type t a <- (getType &&& id -> (t,a)) where
    Type t a = setType t a

class HasUIProp a where
    getUI :: a -> Bool
    setUI :: Bool -> a -> a

pattern UI :: HasUIProp a => a -> a
pattern UI a <- (getUI &&& id -> (True,a)) where
    UI a = setUI True a

class HasUnitProp a where
    getUnit :: a -> Txt
    setUnit :: Txt -> a -> a

pattern Unit :: HasUnitProp a => Txt -> a -> a
pattern Unit u a <- (getUnit &&& id -> (u,a)) where
    Unit u a = setUnit u a

class HasUnmountOnHideProp a where
    getUnmountOnHide :: a -> Bool
    setUnmountOnHide :: Bool -> a -> a

pattern UnmountOnHide :: HasUnmountOnHideProp a => a -> a
pattern UnmountOnHide a <- (getUnmountOnHide &&& id -> (True,a)) where
    UnmountOnHide a = setUnmountOnHide True a

class HasUnstackableProp a where
    getUnstackable :: a -> Bool
    setUnstackable :: Bool -> a -> a

pattern Unstackable :: HasUnstackableProp a => a -> a
pattern Unstackable a <- (getUnstackable &&& id -> (True,a)) where
    Unstackable a = setUnstackable True a

class HasUpwardProp a where
    getUpward :: a -> Bool
    setUpward :: Bool -> a -> a

pattern Upward :: HasUpwardProp a => a -> a
pattern Upward a <- (getUpward &&& id -> (True,a)) where
    Upward a = setUpward True a

class HasURLProp a where
    getURL :: a -> Txt
    setURL :: Txt -> a -> a

pattern URL :: HasURLProp a => Txt -> a -> a
pattern URL u a <- (getURL &&& id -> (u,a)) where
    URL u a = setURL u a

class HasValueProp a where
    type ValueProp a :: *
    type ValueProp a = Txt
    getValue :: a -> ValueProp a
    setValue :: ValueProp a -> a -> a

pattern Value :: HasValueProp a => ValueProp a -> a -> a
pattern Value t a <- (getValue &&& id -> (t,a)) where
    Value t a = setValue t a

class HasVerticalProp a where
    getVertical :: a -> Bool
    setVertical :: Bool -> a -> a

pattern Vertical :: HasVerticalProp a => a -> a
pattern Vertical a <- (getVertical &&& id -> (True,a)) where
    Vertical a = setVertical True a

class HasVerticalAlignProp a where
    getVerticalAlign :: a -> Txt
    setVerticalAlign :: Txt -> a -> a

pattern VerticalAlign :: HasVerticalAlignProp a => Txt -> a -> a
pattern VerticalAlign v a <- (getVerticalAlign &&& id -> (v,a)) where
    VerticalAlign v a = setVerticalAlign v a

class HasVisibleProp a where
    getVisible :: a -> Bool
    setVisible :: Bool -> a -> a

pattern Visible :: HasVisibleProp a => Bool -> a -> a
pattern Visible b a <- (getVisible &&& id -> (b,a)) where
    Visible b a = setVisible b a

class HasWarningProp a where
    getWarning :: a -> Bool
    setWarning :: Bool -> a -> a

pattern Warning :: HasWarningProp a => Bool -> a -> a
pattern Warning w a <- (getWarning &&& id -> (w,a)) where
    Warning w a = setWarning w a

class HasWideProp a where
    getWide :: a -> Maybe Txt
    setWide :: Maybe Txt -> a -> a

pattern Wide :: HasWideProp a => Maybe Txt -> a -> a
pattern Wide w a <- (getWide &&& id -> (w,a)) where
    Wide w a = setWide w a

class HasWidthProp a where
    type WidthProp a :: *
    type WidthProp a = Width
    getWidth :: a -> WidthProp a
    setWidth :: WidthProp a -> a -> a

pattern Width :: HasWidthProp a => WidthProp a -> a -> a
pattern Width w a <- (getWidth &&& id -> (w,a)) where
    Width w a = setWidth w a

class HasWidthsProp a where
    getWidths :: a -> Width
    setWidths :: Width -> a -> a

pattern Widths :: HasWidthsProp a => Width -> a -> a
pattern Widths w a <- (getWidths &&& id -> (w,a)) where
    Widths w a = setWidths w a

class HasWithModalProp a where
    type WithModalProp a
    getWithModal :: a -> WithModalProp a
    setWithModal :: WithModalProp a -> a -> a

pattern WithModal :: HasWithModalProp a => WithModalProp a -> a -> a
pattern WithModal wm a <- (getWithModal &&& id -> (wm,a)) where
    WithModal wm a = setWithModal wm a

class HasWithPortalProp a where
    type WithPortalProp a
    getWithPortal :: a -> WithPortalProp a
    setWithPortal :: WithPortalProp a -> a -> a

pattern WithPortal :: HasWithPortalProp a => WithPortalProp a -> a -> a
pattern WithPortal wp a <- (getWithPortal &&& id -> (wp,a)) where
    WithPortal wp a = setWithPortal wp a

class HasWithRefProp a where
    type WithRefProp a
    getWithRef :: a -> WithRefProp a
    setWithRef :: WithRefProp a -> a -> a

pattern WithRef :: HasWithRefProp a => WithRefProp a -> a -> a
pattern WithRef wr a <- (getWithRef &&& id -> (wr,a)) where
    WithRef wr a = setWithRef wr a

class HasWithTransitionProp a where
    type WithTransitionProp a
    getWithTransition :: a -> WithTransitionProp a
    setWithTransition :: WithTransitionProp a -> a -> a

pattern WithTransition :: HasWithTransitionProp a => WithTransitionProp a -> a -> a
pattern WithTransition wp a <- (getWithTransition &&& id -> (wp,a)) where
    WithTransition wp a = setWithTransition wp a

class HasWrappedProp a where
    getWrapped :: a -> Bool
    setWrapped :: Bool -> a -> a

pattern Wrapped :: HasWrappedProp a => a -> a
pattern Wrapped a <- (getWrapped &&& id -> (True,a)) where
    Wrapped a = setWrapped True a

infixr 0 <|
(<|) f = f

infixr 0 <||>
(<||>) c cs = c <| def |> cs

pattern ToLeft = "left"
pattern ToRight = "right"
pattern ToTop = "top"
pattern ToBottom = "bottom"

pattern Middle = "middle"
pattern Bottom = "bottom"
pattern Top = "top"

pattern Mobile = "mobile"
pattern Tablet = "tablet"
pattern Computer = "computer"
pattern LargeScreen = "large screen"
pattern Widescreen = "widescreen"

pattern Red = "red"
pattern Orange = "orange"
pattern Yellow = "yellow"
pattern Olive = "olive"
pattern Green = "green"
pattern Teal = "teal"
pattern Blue = "blue"
pattern Violet = "violet"
pattern Purple = "purple"
pattern Pink = "pink"
pattern Brown = "brown"
pattern Grey = "grey"
pattern Gray = "grey"
pattern Black = "black"

pattern Mini c = Size "mini" c
pattern Tiny c = Size "tiny" c
pattern Small c = Size "small" c
pattern Medium c = Size "medium" c
pattern Large c = Size "large" c
pattern Big c = Size "big" c
pattern Huge c = Size "huge" c
pattern Massive c = Size "massive" c

pattern Unaligned c = TextAlign "" c
pattern LeftAligned c = TextAlign "left aligned" c
pattern RightAligned c = TextAlign "right aligned" c
pattern CenterAligned c = TextAlign "center aligned" c
pattern Justified c = TextAlign "justified" c

pattern Scale = "scale"
pattern Fade = "fade"
pattern FadeUp = "fade up"
pattern FadeDown = "fade down"
pattern FadeLeft = "fade left"
pattern FadeRight = "fade right"
pattern HorizontalFlip = "horizontal flip"
pattern VerticalFlip = "vertical flip"
pattern Drop = "drop"
pattern FlyLeft = "fly left"
pattern FlyRight = "fly right"
pattern FlyUp = "fly up"
pattern FlyDown = "fly down"
pattern SwingLeft = "swing left"
pattern SwingRight = "swing right"
pattern SwingUp = "swing up"
pattern SwingDown = "swing down"
pattern Browse = "browse"
pattern BrowseRight = "browse right"
pattern SlideDown = "slide down"
pattern SlideUp = "slide up"
pattern SlideRight = "slide right"

pattern Jiggle = "jiggle"
pattern Flash = "flash"
pattern Shake = "shake"
pattern Pulse = "pulse"
pattern Tada = "tada"
pattern Bounce = "bounce"

pattern MediumRectangle = "medium rectangle"
pattern LargeRectangle = "large rectangle"
pattern VerticalRectangle = "vertical rectangle"
pattern SmallRectangle = "small rectangle"
pattern MobileBanner = "mobile banner"
pattern Banner = "banner"
pattern VerticalBanner = "vertical banner"
pattern TopBanner = "top banner"
pattern HalfBanner = "half banner"
pattern ButonAd = "button"
pattern SquareButton = "square button"
pattern SmallButton = "small button"
pattern Skyscraper = "skyscraper"
pattern WideSkyscraper = "wide skyscraper"
pattern Leaderboard = "leaderboard"
pattern LargeLeaderboard = "large leaderboard"
pattern MobileLeaderboard = "mobile leaderboard"
pattern Billboard = "billboard"
pattern Panorama = "panorama"
pattern Netboard = "netboard"
pattern HalfPage = "half page"
pattern Square = "square"
pattern SmallSquare = "small square"
