@Part(title, Root="ada.mss")

@comment{$Source: e:\\cvsroot/ARM/Source/title.mss,v $}
@comment{$Revision: 1.57 $ $Date: 2012/05/19 02:05:52 $}

@begin{ISOOnly}
@Noparanum@Right{@Swiss{@Grow{@Chg{Version=[3],New=[@b{ISO/IEC JTC 1/SC 22 N}],Old=[@B{INTERNATIONAL STANDARD} ISO/IEC 8652:@Chg{Version=[2],New=[2007(E) Ed. 3],Old=[1995(E)@Chg{Version=[1], New=[ with COR.1:2001], Old=[]}]}]}}}}
@Comment{CD draft text}

@Noparanum@Right{@Swiss{@Chg{Version=[3],New=[Date: 2012-03-30],Old=[]}}}

@Noparanum@Right{@Swiss{@Grow{@Chg{Version=[3],New=[@B{ISO/IEC CD 8652}],Old=[]}}}}

@Noparanum@Right{@Swiss{@Chg{Version=[3],New=[ISO/IEC JTC 1/SC 22/WG 9],Old=[]}}}
@end{ISOOnly}
@begin{NotISO}
@Noparanum@Right{@Swiss{@Grow{@B{Ada Reference Manual}, ISO/IEC 8652:@Chg{Version=[3],New=[201z],Old=[@Chg{Version=[2],New=[2007(E) Ed. 3],Old=[1995(E)@Chg{Version=[1], New=[ with COR.1:2001], Old=[]}]}]}}}}
@end{NotISO}

@Noparanum@ @*
@ @*
@ @*

@Comment{Final header...
@begin{ISOOnly}
@Noparanum@swiss{@shrink{@shrink{@shrink{INTERNATIONAL ORGANIZATION FOR STANDARDIZATION}}}}

@Noparanum@swiss{@shrink{@shrink{@shrink{INTERNATIONAL ELECTROTECHNICAL COMMISSION}}}}
@end{ISOOnly}
End comment}

@Noparanum@ @*
@ @*

@begin{ISOOnly}
@Noparanum@Swiss{@b{@grow{@grow{@grow{@grow{Information technology @Em Programming languages @Em Ada}}}}}}
@ @*
@Noparanum@Swiss{@i{Technologies de l'information @Em Langages de programmation @Em Ada}}
@end{ISOOnly}

@Noparanum@ @;@comment{A dummy paragraph containing just a blank}

@begin{NotISO}
@Noparanum@ @*@;@comment{A dummy paragraph containing three blank lines}
@ @*
@ @*
@ @*

@end{NotISO}

@begin{ISOOnly}
@Comment{Jim Moore wanted this deleted in the consolidated editions, as it is confusing.}
@Noparanum@Chg{Version=[3],New=[@Swiss{Revision of second edition (ISO 8652:1995)}],Old=[@Chg{New=[@ @*@Comment{Dummy paragraph}],
               Old=[@Swiss{[Revision of first edition (ISO 8652:1987)]}]}]}
@end{ISOOnly}

@Noparanum@ @*

@begin{NotISO}
@begin{RMOnly}
@Noparanum@Heading{Ada Reference Manual}
@end{RMOnly}
@begin{AARMOnly}
@Noparanum@Heading{Annotated Ada Reference Manual}
@end{AARMOnly}

@Noparanum@ @;@comment{A dummy paragraph containing just a blank}

@Noparanum@Center{@Swiss{@Grow{ISO/IEC 8652:@Chg{Version=[3],New=[201x],Old=[1995]}(E)@Chg{Version=[3],New=< (Submission Draft [Draft 18])>,Old=[]}}}}
@Noparanum@Center{@Swiss{@Chg{Version=[3],New=[],Old=[@Chg{Version=[1], New=[@Grow{with Technical Corrigendum 1}], Old=[]}]}}}
@Noparanum@Center{@Swiss{@Chg{Version=[3],New=[],Old=[@Chg{Version=[2], New=[@Grow{and Amendment 1}], Old=[]}]}}}
@end{NotISO}

@Noparanum@ @;@comment{A dummy paragraph containing just a blank}

@begin{NotISO}
@Noparanum@Center{@Swiss{@Grow{Language and Standard Libraries}}}

@Noparanum@Comment{The following puts the copyright near the bottom of the page}
@ @*@*@*@*@*@*@*

@Noparanum@;Copyright @Latin1(169) 1992, 1993, 1994, 1995  Intermetrics, Inc.

@Noparanum@;@Chg{Version=[1], New=[Copyright @Latin1(169) 2000  The MITRE Corporation, Inc.], Old=[]}

@Noparanum@;@Chg{Version=[2], New=[Copyright @Latin1(169) 2004, 2005, 2006  AXE Consultants], Old=[]}

@Noparanum@;@Chg{Version=[2], New=[Copyright @Latin1(169) 2004, 2005, 2006  Ada-Europe], Old=[]}

@Noparanum@;@Chg{Version=[3], New=[Copyright @Latin1(169) 2008, 2009, 2010, 2011, 2012  AXE Consultants], Old=[]}
@end{NotISO}
@begin{ISOOnly}
@ThinLine
@Noparanum@Center{@Swiss{@Grow{@b{Warning}}}}

@Noparanum@;This document is not an ISO International Standard. It is
distributed for review and comment. It is subject to change without notice and
may not be referred to as an International Standard.

@Noparanum@;Recipients of this draft are invited to submit, with their comment,
notification of any relevant patent rights of which they are aware and to
provide supporting documentation.
@ThinLine
@end{ISOOnly}

@NewPage
@begin{NotISO}
@Comment{The following puts the copyright near the bottom of the page}
@Noparanum@ @*@*@*@*@*

@Noparanum@;@b{Ada Reference Manual - Language and Standard Libraries}

@Noparanum@;Copyright @Latin1(169) 1992, 1993, 1994, 1995, Intermetrics, Inc.

@Noparanum@;This copyright is assigned to the U.S. Government.  All rights reserved.

@Noparanum@;This document may be copied, in whole or in part, in any form or by any means,
as is or with alterations, provided that (1) alterations are clearly marked as
alterations and (2) this copyright notice is included unmodified in any copy.
Compiled copies of standard library units and examples need not contain this
copyright notice so long as the notice is included in all copies of source code
and documentation.

@ThinLine

@Noparanum@;@ @;@comment{A dummy paragraph containing just a blank}

@Noparanum@;@Chg{Version=[1], New=[@b{Technical Corrigendum 1}], Old=[]}

@Noparanum@;@Chg{Version=[1], New=[Copyright @Latin1(169) 2000, The MITRE Corporation.  All Rights Reserved.], Old=[]}

@Noparanum@;@Chg{Version=[1], New=[This document may be copied, in whole or in part, in any form or by any means,
as is, or with alterations, provided that (1) alterations are clearly marked as
alterations and (2) this copyright notice is included unmodified in any copy.
Any other use or distribution of this document is prohibited without the prior
express permission of MITRE.], Old=[]}

@Noparanum@;@Chg{Version=[1], New=[You use this document on the condition that
you indemnify and hold harmless MITRE, its Board of Trustees, officers, agents,
and employees, from any and all liability or damages to yourself or your
hardware or software, or third parties, including attorneys' fees, court costs,
and other related costs and expenses, arising out of your use of this document
irrespective of the cause of said liability.], Old=[]}

@Noparanum@;@Chg{Version=[1], New=[MITRE MAKES THIS DOCUMENT AVAILABLE ON AN "AS IS" BASIS AND MAKES NO WARRANTY,
EXPRESS OR IMPLIED, AS TO THE ACCURACY, CAPABILITY, EFFICIENCY MERCHANTABILITY,
OR FUNCTIONING OF THIS DOCUMENT.  IN NO EVENT WILL MITRE BE LIABLE FOR ANY
GENERAL, CONSEQUENTIAL, INDIRECT, INCIDENTAL, EXEMPLARY, OR SPECIAL DAMAGES,
EVEN IF MITRE HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.], Old=[]}

@Noparanum@;@ @;@comment{A dummy paragraph containing just a blank}

@Noparanum@;@Chg{Version=[2], New=[@b{Amendment 1}], Old=[]}

@Noparanum@;@Chg{Version=[2], New=[Copyright @Latin1(169) 2004, 2005, 2006@Chg{Version=[3], New=[, 2007],Old=[]}, AXE Consultants.  All Rights Reserved.], Old=[]}

@Noparanum@;@Chg{Version=[2], New=[This document may be copied, in whole or in part, in any form or by any means,
as is, or with alterations, provided that (1) alterations are clearly marked as
alterations and (2) this copyright notice is included unmodified in any copy.
Any other use or distribution of this document is prohibited
without the prior express permission of AXE.], Old=[]}

@Noparanum@;@Chg{Version=[2], New=[You use this document on the condition that
you indemnify and hold harmless AXE, its board, officers, agents, and
employees, from any and all liability or damages to yourself or your hardware
or software, or third parties, including attorneys' fees, court costs, and
other related costs and expenses, arising out of your use of this document
irrespective of the cause of said liability.], Old=[]}

@Noparanum@;@Chg{Version=[2], New=[AXE MAKES THIS DOCUMENT AVAILABLE ON AN "AS IS" BASIS AND MAKES NO WARRANTY,
EXPRESS OR IMPLIED, AS TO THE ACCURACY, CAPABILITY, EFFICIENCY MERCHANTABILITY,
OR FUNCTIONING OF THIS DOCUMENT. IN NO EVENT WILL AXE BE LIABLE FOR ANY
GENERAL, CONSEQUENTIAL, INDIRECT, INCIDENTAL, EXEMPLARY, OR SPECIAL DAMAGES,
EVEN IF AXE HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.], Old=[]}

@Noparanum@;@Chg{Version=[3], New=[@b{Third Edition}], Old=[]}

@Noparanum@;@Chg{Version=[3], New=[Copyright @Latin1(169) 2008, 2009, 2010, 2011, 2012 AXE Consultants.  All Rights Reserved.], Old=[]}

@Noparanum@;@Chg{Version=[3], New=[This document may be copied, in whole or in part, in any form or by any means,
as is, or with alterations, provided that (1) alterations are clearly marked as
alterations and (2) this copyright notice is included unmodified in any copy.
Any other use or distribution of this document is prohibited
without the prior express permission of AXE.], Old=[]}

@Noparanum@;@Chg{Version=[3], New=[You use this document on the condition that
you indemnify and hold harmless AXE, its board, officers, agents, and
employees, from any and all liability or damages to yourself or your hardware
or software, or third parties, including attorneys' fees, court costs, and
other related costs and expenses, arising out of your use of this document
irrespective of the cause of said liability.], Old=[]}

@Noparanum@;@Chg{Version=[3], New=[AXE MAKES THIS DOCUMENT AVAILABLE ON AN "AS IS" BASIS AND MAKES NO WARRANTY,
EXPRESS OR IMPLIED, AS TO THE ACCURACY, CAPABILITY, EFFICIENCY MERCHANTABILITY,
OR FUNCTIONING OF THIS DOCUMENT. IN NO EVENT WILL AXE BE LIABLE FOR ANY
GENERAL, CONSEQUENTIAL, INDIRECT, INCIDENTAL, EXEMPLARY, OR SPECIAL DAMAGES,
EVEN IF AXE HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.], Old=[]}

@Noparanum@;@ @;@comment{A dummy paragraph containing just a blank}

@Noparanum@;@Chg{Version=[2], New=[@b{Ada 2005 Consolidated Standard}], Old=[]}

@Noparanum@;@Chg{Version=[2], New=[Copyright @Latin1(169) 2004, 2005, 2006, Ada-Europe.], Old=[]}

@Noparanum@;@Chg{Version=[2], New=[This document may be copied, in whole or in
part, in any form or by any means, as is, or with alterations, provided that
(1) alterations are clearly marked as alterations and (2) this copyright notice
is included unmodified in any copy. Any other use or distribution of this
document is prohibited without the prior express permission of Ada-Europe.], Old=[]}

@Noparanum@;@Chg{Version=[2], New=[You use this document on the condition that
you indemnify and hold harmless Ada-Europe and its Board from any and all
liability or damages to yourself or your hardware or software, or third
parties, including attorneys' fees, court costs, and other related costs and
expenses, arising out of your use of this document irrespective of the cause of
said liability.], Old=[]}

@Noparanum@;@Chg{Version=[2], New=[ADA-EUROPE MAKES THIS DOCUMENT AVAILABLE ON AN "AS IS" BASIS AND MAKES NO WARRANTY,
EXPRESS OR IMPLIED, AS TO THE ACCURACY, CAPABILITY, EFFICIENCY MERCHANTABILITY,
OR FUNCTIONING OF THIS DOCUMENT. IN NO EVENT WILL ADA-EUROPE BE LIABLE FOR ANY
GENERAL, CONSEQUENTIAL, INDIRECT, INCIDENTAL, EXEMPLARY, OR SPECIAL DAMAGES,
EVEN IF ADA-EUROPE HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.], Old=[]}
@end{NotISO}

@begin{ISOOnly}
@ThinLine
@Noparanum@Center{@Swiss{@Grow{@Grow{@b{Copyright Notice}}}}}

@Noparanum@;This ISO document is a working draft or committee draft and is
copyright-protected by ISO. While the reproduction of working drafts or
committee drafts in any form for use by participants in the ISO standards
development process is permitted without prior permission from ISO, neither this
document nor any extract from it may be reproduced, stored, or transmitted in
any form for any other purpose without prior written permission from ISO.

@Noparanum@;Reproduction for sales purposes may be subject to royalty payments or a
licensing agreement.

@Noparanum@;Violators may be prosecuted.
@ThinLine
@end{ISOOnly}
