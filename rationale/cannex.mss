@Part(xxx, Root="rat.msm")

@comment($Source: e:\\cvsroot/ARM/Rationale/cannex.mss,v $)
@comment($Revision: 1.1 $ $Date: 2006/02/07 07:47:51 $)

@LabeledClause{Summary table}

This paper concludes with a table showing at a glance the
various facilities in the six main containers.

@leading@;In order to save space the following abbreviations are used in the
table:

@begin[Example]
@Tabset[P7,P35,P42]
T@\container type eg Map@\H_T@\Hash_Type
C: T@\Container: container type@\I_T@\Index_Type
P: C@\Position: Cursor@\K_T@\Key_Type
L, R@\Left, Right@\Ex_Index@\Extended_Index
C_T@\Count_Type@\B@\Boolean
E_T@\Element_Type
@end[Example]


also Index @en means that another subprogram exists with similar parameters
except that the first parameters are of type Vector and Index_Type
(or Extended_Index) rather than those involving cursors.

also Key and also Element similarly apply to maps and sets respectively.

*** Stopped here *** (How the heck am I going to turn this into a table again?)

\trowd \trgaph108\trleft-108\trkeep\trhdr\trbrdrt\brdrs\brdrw10 \trbrdrl\brdrs\brdrw10
\trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10 \trbrdrh\brdrs\brdrw10
\trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560
\pard\plain \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\cell
}\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
vectors\cell lists\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
hashed maps\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
ordered maps\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
hashed sets\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
ordered sets\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trhdr\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 @b[generic\cell
]\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033    type
Index_Type is range <>;\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033    type
Key_Type is private;\cell \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033    type
Element_Type is private;\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033    with
function Hash( ... ) return Hash_Type;\cell \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
on Key\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
on Element\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033    with
function Equivalent_...(L, R: ...) return Boolean;\cell \cell \pard
\s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
on Key\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
on Element\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033    with
function "<" (L, R: ... ) return Boolean is <>;\cell \cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell on Key\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
on Element\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033    with
function "=" (L, R: E_T) return B is <>;\cell Y\cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 @b[package]
Ada.Containers.... is\cell Vectors\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Doubly_
Linked_
Lists\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Hashed_
Maps\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Ordered_
Maps\cell Hashed_
Sets\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Ordered_
Sets\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 pragma
Preelaborate( ... );\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Equivalent_...(L, R: ...) return Boolean;  \cell \cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell on Key\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
on Element\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 subtype
Extended_Index ...

No_Index: constant Ex_Ind := Ex_Ind'First;\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 type T
is tagged private;
pragma Preelaborable_Initialization(T);\cell Vector\cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
List\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Map\cell Map\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Set\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Set\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 type Cursor
is private;
pragma Preelaborable_Initialization(Cursor);\cell Y\cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 Empty_T:
constant T;\cell Vector\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
List\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Map\cell Map\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Set\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Set\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 No_Element:
constant Cursor;\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
"=" (Left, Right: T) return Boolean;\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Equivalent_Sets(L, R: Set) return Boolean;

function To_Set(New_Item: E_T) return Set;\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
To_Vector(Length: C_T) return Vector;

function To_Vector(New_Item: E_T;
                               Length: C_T) return Vector;\cell \pard
\s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
"&" (L, R: Vector) return Vector;

function "&" (L: Vector; R: E_T) return Vector;

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
function "&" (L: E_T; R: Vector) return Vector;

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
function "&" (L, R: E_T) return Vector;\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Capacity(C: T) return C_T;

procedure Reserve_Capacity(C: T; Capacity: C_T);\cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell Y\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Length(C: T) return Count_Type;\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Set_Length(C: in out T; Length: in C_T);\cell Y\cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Is_Empty(C: T) return B;

procedure Clear(C: in out T);\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
To_Cursor(C: Vector; Index: Ex_Ind)
                                return Cursor;

function To_Index(P: C) return Ex_Ind;\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Key(P: C) return K_T;\cell \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Element(P: C) return E_T;\cell Y

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
also Index\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Replace_Element(C: in out T; P: C;
                                              New_Item: E_T);\cell
Y

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
also Index\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Query_Element(P: C;
   Process: not null acc proc( ... ) );\cell in Element

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
also Index\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
in Element\cell in Key,
in Element\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
in Key,
in Element\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
in Element\cell in Element\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Update_Element(C: in out T; P: C;
   Process: not null acc proc( ... ) );\cell in out Elem

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
also Index\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
in out Elem\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
in Key,
in out Elem\cell in Key,
in out Elem\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Move(Target, Source: in out T);\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Insert(C: in out Vector; Before: Ex_Ind;
                           New_Item: Vector);

procedure Insert(C: in out Vector; Before: Cursor;
                           New_Item: Vector);

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
procedure Insert(C: in out Vector; Before: Cursor;
                       New_Item: Vector; Position: out Cursor);\cell
\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y \cell \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Insert(C: in out T; Before: C;
                           New_Item: E_T; Count: C_T := 1);\cell Y

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
also Index\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Insert(C: in out T; Before: C;
                           New_Item: E_T; Position: out Cursor;
                           Count: C_T := 1);\cell Y\cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Insert(C: in out T; Before: C;
                          Position: out Cursor; Count: C_T := 1);

element has default value\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
also Index\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Insert(C: in out T; Key: K_T;
                           New_Item: E_T; Position: out Cursor;
                           Inserted: out B);\cell \cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y (no key)\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y (no key)\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Insert(C: in out T; Key: K_T;
                           Position: out Cursor; Inserted: out B);

element has default value\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell Y\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Insert(C: in out T; Key: K_T;
                           New_Item: E_T);\cell \cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y (no key)\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y (no key)\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Prepend(C: in out Vector;
                                New_Item: Vector);\cell Y\cell \pard
\s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Prepend(C: in out T;
                                New_Item: E_T; Count: C_T := 1); \cell
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Append(C: in out Vector;
                               New_Item: Vector);\cell Y\cell \pard
\s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Append(C: in out T;
                               New_Item: E_T; Count: C_T := 1); \cell
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Insert_Space(C: in out V; Before: Cursor;
                          Position: out Cursor; Count: C_T := 1);\cell
Y

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
also Index\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Include(C: in out T;
                              Key: Key_Type; New_Item: E_T);\cell
\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y (no key)\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y (no key)\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Replace(C: in out T;
                              Key: Key_Type; New_Item: E_T);\cell
\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y (no key)\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y (no key)\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Exclude(C: in out T;
                               Key: Key_Type);\cell \cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y (Item not key)\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y (item not key) \cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Delete(C: in out T; P: in out C;
                             Count: C_T := 1);\cell Y

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
also Index\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y (no count)

also Key\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y (no count)

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
also Key\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y (no count)

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
also Element\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y (no count)

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
also Element\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Delete_First(C: in out T; Count: C_T := 1);

procedure Delete_Last(C: in out T; Count: C_T := 1);\cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y (no count)\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell Y (no count)\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Reverse_Elements(C: in out T);\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Swap(C: in out T; I, J: Cursor);\cell Y

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
also Index\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Swap_Links(C: in out List; I, J: Cursor);\cell \cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Splice(Target: in out List; Before: Cursor;
                            Source: in out List);

procedure Splice(Target: in out List; Before: Cursor;
                   Source: in out List; Position: in out Cursor);

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
procedure Splice{\expnd0\expndtw-2 (Container: in out List; Before:
Cursor;}
                             Position: in out Cursor);\cell \pard
\s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Union(Target: in out Set; Source: Set);

function Union(L, R: Set) return Set;

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
function "or" (L, R: Set) return Set renames Union;\cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y \cell Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Intersection(Target: in out Set;
                                     Source: Set);

function Intersection(L, R: Set) return Set;

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
function "and" (L, R: Set) return Set
                        renames Intersection;\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Difference(Target: in out Set; Source: Set);

function Difference(L, R: Set) return Set;

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
function "@en" (L, R: Set) return Set renames Difference;\cell \pard
\s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell Y \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\trowd \trgaph108\trrh518\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Symmetric_Difference(Target: in out Set;
                                                      Source: Set);

function Symmetric_Difference (L, R: Set) return Set;

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
function "xor" (L, R: Set) return Set
                       renames Symmetric_Difference;\cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell Y\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trrh518\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\trowd \trgaph108\trrh374\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Overlap(L, R: Set) return Boolean;

function Is_Subset(Subset: Set; Of_Set: Set) return B;\cell \pard
\s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y \cell Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trrh374\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
First_Index(C: T) return Index_Type;\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
First(C: T) return Cursor;\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
First_Element(C: T) return Element_Type;\cell Y\cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell Y\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
First_Key(C: T) return Key_Type;\cell \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell Y\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Last_Index(C: T) return Ex_Ind;\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Last(C: T) return Cursor;\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell Y\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Last_Element(C: T) return Element_Type;\cell Y\cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell Y\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Last_Key(C: T) return Key_Type;\cell \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell Y\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Next(P: C) return Cursor;

procedure Next(P: in out C);\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Previous(P: C) return Cursor;

procedure Previous(P: in out C);\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell Y\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Find_Index(C: T; Item: E_T;
                            Index: I_T := I_T'First) return Ex_Ind;\cell
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Find(C: T; ... ; P: C := No_Element)
                      return Cursor;\cell Element\cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Element\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Key (no position)\cell Key (no position)\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Element (no position)\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Element (no position)\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Element(C: T; Key: K_T) return E_T;\cell \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Reverse_Find_Index(C: T; Item: E_T;
                            Index: I_T := I_T'First) return Ex_Ind;\cell
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Reverse_Find(C: T; ... ; P: C := No_Element)
                                     return Cursor;\cell Element\cell
\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Element\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Floor(C: T; ...) return Cursor;

function Ceiling(C: T; ...) return Cursor;\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Key: K_T\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Item: E_T\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Contains(C: T; ...) return Boolean;\cell Element\cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Element\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Key \cell Key \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Element \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Element\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Has_Element(P: C) return Boolean;\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Equivalent_... (L, R: Cursor) return Boolean;

function Equivalent_... ({\expnd0\expndtw-2 L: Cursor; R:...) return
Boolean};

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
f{\expnd0\expndtw-2 unction Equivalent_... (L:...; R: Cursor) return
Boolean};\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell Keys \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Elements \cell \cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
"<" (L, R: Cursor) return Boolean;

function ">" (L, R: Cursor) return Boolean;

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
function "<" (L, Cursor; R: ...) return Boolean;

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
function ">" (L, Cursor; R: ...) return Boolean;

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
function "<" (L:...; R: Cursor) return Boolean;

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
function ">" (L:...; R: Cursor) return Boolean;\cell \cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell Key \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Element \cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Iterate(C: in T;
   Process: not null acc proc (P: C) );\cell Y\cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Reverse_Iterate(C: in T;
   Process: not null acc proc (P: C) );\cell Y\cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell Y\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 @b[generic]
   with function "<" (Left, Right: E_T) return B is <>;
@b[package] Generic_Sorting is
   function Is_Sorted(C: T) return Boolean;
   procedure Sort(C: in out T);
   procedure Merge(Target, Source: in out T);
@b[end] Generic_Sorting;\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 @b[generic]
   type Key_Type (<>) is private;\cell \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033    with
function Key(Element: E_T) return Key_Type;{\cs20\b\fs20 \cell }{\cell
}\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y \cell Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033    with
function Hash(Key: K_T) return Hash_Type;\cell \cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033    with
function Equivalent_Keys (L, R: Key_Type)
                                                     return Boolean;@b[\cell
]{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033    with
function "<" (L, R: Key_Type) return B is <>;\cell \cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 @b[package]
Generic_Keys is\cell \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Equivalent_Keys(L, R: Key_Type) return B; \cell \cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Key(P: C) return Key_Type;\cell \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Element(C: T; Key: K_T) return Element_T;\cell \cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Replace(C: in out T; Key: Key_Type;
                                New_Item: E_T);

procedure Exclude(C: in out T; Key: Key_Type);

\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
procedure Delete(C: in out T; Key: Key_Type);\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell Y\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Find(C: T; Key: K_T) return Cursor;\cell \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Floor(C: T; Key: K_T) return Cursor;

function Ceiling(C: T; Key: K_T) return Cursor;@b[\cell ]\pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y \cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 function
Contains(C: T; Key: K_T) return Boolean;\cell \cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 procedure
Update_Element_Preserving_Key
   (C: in out T; P: C;
   Process: not null acc proc (Element: in out E_T) );\cell \cell
\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 @b[end]
Generic_Keys;\cell \cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
{\cell \cell }\pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\pard\plain \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\f1\fs16\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 @b[private]{\cs20\b\fs20

}   ...@i[ -- not specified by the language]
@b[end] Ada.Containers....;{\cs20\b\fs20 \cell }Y\cell \pard \s43\ql
\li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell \pard \s43\ql \li0\ri0\sb40\sa40\sl-200\slmult0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
Y\cell Y\cell \pard\plain \ql \li0\ri0\widctlpar\intbl\aspalpha\aspnum\faauto\adjustright\rin0\lin0
\fs20\lang2057\langfe1033\cgrid\langnp2057\langfenp1033 {\trowd \trgaph108\trleft-108\trkeep\trbrdrt\brdrs\brdrw10
\trbrdrl\brdrs\brdrw10 \trbrdrb\brdrs\brdrw10 \trbrdrr\brdrs\brdrw10
\trbrdrh\brdrs\brdrw10 \trbrdrv\brdrs\brdrw10 \trftsWidth3\trwWidth10668\trftsWidthB3\trftsWidthA3\trautofit1\trpaddl108\trpaddr108\trpaddfl3\trpaddft3\trpaddfb3\trpaddfr3
\clvertalt\clbrdrt\brdrs\brdrw10 \clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10
\clbrdrr\brdrs\brdrw10 \cltxlrtb\clftsWidth3\clwWidth4111 \cellx4003\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1092 \cellx5095\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx6188\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx7281\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx8374\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx9467\clvertalt\clbrdrt\brdrs\brdrw10
\clbrdrl\brdrs\brdrw10 \clbrdrb\brdrs\brdrw10 \clbrdrr\brdrs\brdrw10
\cltxlrtb\clftsWidth3\clwWidth1093 \cellx10560\row }
