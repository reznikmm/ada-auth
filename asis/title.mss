@Part(title, Root="asis.mss")

@comment{$Source: e:\\cvsroot/ARM/ASIS/title.mss,v $}
@comment{$Revision: 1.4 $ $Date: 2010/01/29 05:17:56 $}

@begin{ISOOnly}
@Noparanum@Swiss{@Chg{Version=[1],New=[@B{@latin1(169) ISO/IEC 2009 @en All rights reserved}],Old=[]}}
@ @*
@Comment{Final only
@Noparanum@Right{@Swiss{@Grow{@B{INTERNATIONAL STANDARD} ISO/IEC 15291:@Chg{Version=[1],New=[201x(E)],Old=[1999(E)]}}}}
}@comment{end Final only}
@Comment{Draft}
@Noparanum@Right{@Swiss{@Grow{@Chg{Version=[1],New=[@B{ISO/IEC JTC1/SC 22 N}],Old=[@B{INTERNATIONAL STANDARD} ISO/IEC 15291:1999(E)]}}}}

@Noparanum@Right{@Swiss{@Chg{Version=[1],New=[@B{Date: 2009-05-15}],Old=[]}}}

@Noparanum@Right{@Swiss{@Grow{@Chg{Version=[1],New=[@B{ISO/IEC CD 15291}],Old=[]}}}}

@Noparanum@Right{@Swiss{@Chg{Version=[1],New=[@B{ISO/IEC JTC 1/SC 22/WG 9}],Old=[]}}}
@comment{End Draft}
@end{ISOOnly}
@begin{NotISO}
@Noparanum@Right{@Swiss{@Grow{@B{Ada Semantic Interface Specification}, ISO/IEC 15291:@Chg{Version=[1],New=[201x(E), Working Draft 0.8],Old=[1999(E)]}}}}
@end{NotISO}

@Noparanum@ @*
@ @*
@ @*

@comment{Final only
@begin{ISOOnly}
@Noparanum@swiss{@shrink{@shrink{@shrink{INTERNATIONAL ORGANIZATION FOR STANDARDIZATION}}}}

@Noparanum@swiss{@shrink{@shrink{@shrink{INTERNATIONAL ELECTROTECHNICAL COMMISSION}}}}
@end{ISOOnly}
}@comment{end final only}

@Noparanum@ @*
@ @*

@begin{ISOOnly}
@Comment{Should be 14 point}
@Noparanum@Swiss{@Grow{@Grow{@Grow{@Grow{@b{Information technology @Em Programming languages @Em@*Ada Semantic Interface Specification (ASIS)}}}}}}

@Noparanum@Swiss{@i{Technologies de l'information @em Langages de programmation @em Sp@Latin1(233)cification
d'interface pour la s@Latin1(233)mantique Ada}}
@end{ISOOnly}

@Noparanum@ @;@comment{A dummy paragraph containing just a blank}

@begin{NotISO}
@Noparanum@ @*@;@comment{A dummy paragraph containing three blank lines}
@ @*
@ @*
@ @*

@end{NotISO}

@begin{ISOOnly}
@ @*
@Noparanum@Chg{New=[@Swiss{Revision of first edition (ISO 15291:1999)}],
               Old=[@ @*@Comment{Dummy paragraph}]}
@end{ISOOnly}

@Noparanum@ @*

@begin{NotISO}
@begin{RMOnly}
@Noparanum@Heading{Ada Semantic Interface Specification (ASIS)}
@end{RMOnly}
@begin{AARMOnly}
@Noparanum@Heading{Annotated Ada Semantic Interface Specification (ASIS)}
@end{AARMOnly}

@Noparanum@ @;@comment{A dummy paragraph containing just a blank}

@Noparanum@Center{@Swiss{@Chg{Version=[1], New=[@Grow{ISO/IEC 15291:201x(E)}, Working Draft 0.8],Old=[@Grow{ISO/IEC 15291:1999(E)}]}}}
@end{NotISO}

@Noparanum@ @;@comment{A dummy paragraph containing just a blank}

@Comment{This warning goes in all but the final version of the document}
@ @*@*@*
@center{@b{@grow{@grow{Warning}}}}

This document is not an ISO International Standard. It is distributed for
review and comment. It is subject to change without notice and may not be
referred to as an International Standard.

Recipients of this draft are invited to submit, with their comments,
notification of any relevant patent rights of which they are aware and to
provide supporting documentation.

@NewPage
@Comment{For ISO version, the copyright notice goes on the "inside front
cover or back cover". But we don't have a cover, so I am putting it on the
back of the title page.}
@begin{NotISO}
@Comment{The following puts the copyright near the bottom of the page}
@Noparanum@ @*@*@*@*@*



@Comment{Don't have an appropriate copyright.
@Noparanum@;@Chg{Version=[2], New=[@b{Consolidated Standard}], Old=[]}

@Noparanum@;@Chg{Version=[2], New=[Copyright @Latin1(169) 2008, 2009  Ada-Europe.], Old=[]}

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

** End old copyrights.}

@end{NotISO}

@begin{ISOOnly}@Comment{Copyright for drafts}
@center{@b{@grow{@grow{Copyright notice}}}}

This ISO document is a working draft or committee draft and is
copyright-protected by ISO. While the reproduction of working drafts or
committee drafts in any form for use by participants in the ISO standards
development process is permitted without prior permission from ISO, neither this
document nor any extract from it may be reproduced, stored or transmitted in any
form for any other purpose without prior written permission from ISO.

Requests for permission to reproduce this document for the purpose of selling it
should be addressed as shown below or to ISO's member body in the country of the
requester:

@Begin{Indent}
ISO Copyright Office@*
Case postale 56@*
CH-1211 Geneva 20@*
Tel: +41 22 749 01 11@*
Fax: +41 22 749 09 47@*
Email: @urllink{URL=[mailto:copyright@iso.org],Text=[copyright@iso.org]}@*
Web: @urllink{URL=[http://www.iso.org],Text=[www.iso.org]}
@end{Indent}

Reproduction for sales purposes may be subject to royalty payments or a
licensing agreement.

Violators may be prosecuted.
@end{ISOOnly}
