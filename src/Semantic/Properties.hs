module Semantic.Properties (module Semantic.Properties, module Export) where

import Semantic.Properties.Active as Export
import Semantic.Properties.Animated as Export
import Semantic.Properties.As as Export
import Semantic.Properties.Attached as Export
import Semantic.Properties.Attributes as Export
import Semantic.Properties.Avatar as Export
import Semantic.Properties.Basic as Export
import Semantic.Properties.Block as Export
import Semantic.Properties.Bordered as Export
import Semantic.Properties.Bulleted as Export
import Semantic.Properties.Celled as Export
import Semantic.Properties.Centered as Export
import Semantic.Properties.Children as Export
import Semantic.Properties.Circular as Export
import Semantic.Properties.Classes as Export
import Semantic.Properties.Clearing as Export
import Semantic.Properties.Close as Export
import Semantic.Properties.Color as Export
import Semantic.Properties.Corner as Export
import Semantic.Properties.Compact as Export
import Semantic.Properties.Disabled as Export
import Semantic.Properties.Divided as Export
import Semantic.Properties.Dividing as Export
import Semantic.Properties.Empty as Export
import Semantic.Properties.Error as Export
import Semantic.Properties.Fitted as Export
import Semantic.Properties.Flipped as Export
import Semantic.Properties.Floated as Export
import Semantic.Properties.Floating as Export
import Semantic.Properties.Fluid as Export
import Semantic.Properties.Focus as Export
import Semantic.Properties.Focused as Export
import Semantic.Properties.Hidden as Export
import Semantic.Properties.Horizontal as Export
import Semantic.Properties.Indeterminate as Export
import Semantic.Properties.Inline as Export
import Semantic.Properties.Internal as Export
import Semantic.Properties.Inverted as Export
import Semantic.Properties.Labeled as Export
import Semantic.Properties.LabelPosition as Export
import Semantic.Properties.Link as Export
import Semantic.Properties.Loading as Export
import Semantic.Properties.Localize as Export
import Semantic.Properties.Name as Export
import Semantic.Properties.Negative as Export
import Semantic.Properties.OnChange as Export
import Semantic.Properties.OnClick as Export
import Semantic.Properties.Ordered as Export
import Semantic.Properties.Pointing as Export
import Semantic.Properties.Position as Export
import Semantic.Properties.Positive as Export
import Semantic.Properties.Primary as Export
import Semantic.Properties.Relaxed as Export
import Semantic.Properties.Ribbon as Export
import Semantic.Properties.Rotated as Export
import Semantic.Properties.Rounded as Export
import Semantic.Properties.Secondary as Export
import Semantic.Properties.Section as Export
import Semantic.Properties.Selection as Export
import Semantic.Properties.Size as Export
import Semantic.Properties.Spaced as Export
import Semantic.Properties.Sub as Export
import Semantic.Properties.TabIndex as Export
import Semantic.Properties.Tag as Export
import Semantic.Properties.TextAlign as Export

infixl 1 !%
(!%) c cs as = Children (cs (Attributes as c))

infixl 1 %!
(%!) c as cs = Attributes (as (Children cs c))

pattern ToLeft = "left"
pattern ToRight = "right"
pattern ToTop = "top"
pattern ToBottom = "bottom"