@comment{ $Source: e:\\cvsroot/ARM/Source/pre_standard.mss,v $ }
@comment{ $Revision: 1.49 $ $Date: 2023/01/05 05:49:12 $ $Author: randy $ }
@Part(predefstandard, Root="ada.mss")

@Comment{$Date: 2023/01/05 05:49:12 $}

@RMNewPageVer{Version=[2]}@Comment{For printed version of Ada 2005 RM}
@RMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledClause{The Package Standard}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
This @Chg{Version=[3],New=[subclause],Old=[clause]} outlines
the specification of the package Standard
containing all predefined identifiers in the language.
@PDefn{unspecified}
The corresponding package body is not specified by the language.

The operators that are predefined for the types declared in the
package Standard are given in comments since they are implicitly
declared.
@Defn2{Term=[italics],Sec=(pseudo-names of anonymous types)}
Italics are used for pseudo-names of anonymous types (such
as @i{root_real}) and for undefined information (such as
@i{implementation-defined}).
@begin{Ramification}
  All of the predefined operators are of convention Intrinsic.
@end{Ramification}

@end{Intro}

@begin{StaticSem}
@Leading@;The library package Standard has the following declaration:
@ImplDef{The names and characteristics of the numeric subtypes declared in
the visible part of package Standard.}
@begin{Example}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0414-1]}
@RootLibUnit{Standard}@key[package] Standard@Chg{Version=[5],New=[],Old=[ @key[is]]}
   @Chg{Version=[5],New=[@key[with]],Old=[@key[pragma]]} Pure@Chg{Version=[5],New=[ @key[is]],Old=[(Standard);]}

   @key[type] @AdaTypeDefn{Boolean} @key[is] (False, True);


@Keepnext   --@ExamCom{ The predefined relational operators for this type are as follows:}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0028],ARef=[AI95-00145-01]}
   -- @key[function] "="   (Left, Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean;
   -- @key[function] "/="  (Left, Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean;
   -- @key[function] "<"   (Left, Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean;
   -- @key[function] "<="  (Left, Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean;
   -- @key[function] ">"   (Left, Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean;
   -- @key[function] ">="  (Left, Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean;


@Keepnext   --@ExamCom{ The predefined logical operators and the predefined logical}
   --@ExamCom{ negation operator are as follows:}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0028],ARef=[AI95-00145-01]}
   -- @key[function] "@key[and]" (Left, Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean@Chg{New=['Base],Old=[]};
   -- @key[function] "@key[or]"  (Left, Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean@Chg{New=['Base],Old=[]};
   -- @key[function] "@key[xor]" (Left, Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean@Chg{New=['Base],Old=[]};


@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0028],ARef=[AI95-00145-01]}
   -- @key[function] "@key[not]" (Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean@Chg{New=['Base],Old=[]};


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
   --@ExamCom{ The integer type root_integer @Chg{Version=[2],New=[and the],Old=[is predefined.]}}
   --@ExamCom{ @Chg{Version=[2],New=[],Old=[The ]}corresponding universal type @Chg{Version=[2],New=[],Old=[is ]}universal_integer@Chg{Version=[2],New=[ are predefined],Old=[]}.}


   @key[type] @AdaTypeDefn{Integer} @key[is] @key{range} @VirtName{implementation-defined};


   @key[subtype] @AdaSubtypeDefn{Name=[Natural],Of=[Integer]}  @key[is] Integer @key[range] 0 .. Integer'Last;
   @key[subtype] @AdaSubtypeDefn{Name=[Positive],Of=[Integer]} @key[is] Integer @key[range] 1 .. Integer'Last;


@Keepnext   --@ExamCom{ The predefined operators for type Integer are as follows:}

   -- @key[function] "="  (Left, Right : Integer'Base) @key[return] Boolean;
   -- @key[function] "/=" (Left, Right : Integer'Base) @key[return] Boolean;
   -- @key[function] "<"  (Left, Right : Integer'Base) @key[return] Boolean;
   -- @key[function] "<=" (Left, Right : Integer'Base) @key[return] Boolean;
   -- @key[function] ">"  (Left, Right : Integer'Base) @key[return] Boolean;
   -- @key[function] ">=" (Left, Right : Integer'Base) @key[return] Boolean;


   -- @key[function] "+"   (Right : Integer'Base) @key[return] Integer'Base;
   -- @key[function] "-"   (Right : Integer'Base) @key[return] Integer'Base;
   -- @key[function] "@key[abs]" (Right : Integer'Base) @key[return] Integer'Base;


   -- @key[function] "+"   (Left, Right : Integer'Base) @key[return] Integer'Base;
   -- @key[function] "-"   (Left, Right : Integer'Base) @key[return] Integer'Base;
   -- @key[function] "*"   (Left, Right : Integer'Base) @key[return] Integer'Base;
   -- @key[function] "/"   (Left, Right : Integer'Base) @key[return] Integer'Base;
   -- @key[function] "@key[rem]" (Left, Right : Integer'Base) @key[return] Integer'Base;
   -- @key[function] "@key[mod]" (Left, Right : Integer'Base) @key[return] Integer'Base;


   -- @key[function] "**"  (Left : Integer'Base; Right : Natural)
   --                  @key[return] Integer'Base;


   --@ExamCom{ The specification of each operator for the type}
   --@ExamCom{ root_integer, or for any additional predefined integer}
   --@ExamCom{ type, is obtained by replacing Integer by the name of the type}
   --@ExamCom{ in the specification of the corresponding operator of the type}
   --@ExamCom{ Integer. The right operand of the exponentiation operator}
   --@ExamCom{ remains as subtype Natural.}


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
   --@ExamCom{ The floating point type root_real @Chg{Version=[2],New=[and the],Old=[is predefined.]}}
   --@ExamCom{ @Chg{Version=[2],New=[],Old=[The ]}corresponding universal type @Chg{Version=[2],New=[],Old=[is ]}universal_real@Chg{Version=[2],New=[ are predefined],Old=[]}.}


   @key[type] @AdaTypeDefn{Float} @key[is] @key{digits} @VirtName{implementation-defined};


@Keepnext   --@ExamCom{ The predefined operators for this type are as follows:}

   -- @key[function] "="   (Left, Right : Float) @key[return] Boolean;
   -- @key[function] "/="  (Left, Right : Float) @key[return] Boolean;
   -- @key[function] "<"   (Left, Right : Float) @key[return] Boolean;
   -- @key[function] "<="  (Left, Right : Float) @key[return] Boolean;
   -- @key[function] ">"   (Left, Right : Float) @key[return] Boolean;
   -- @key[function] ">="  (Left, Right : Float) @key[return] Boolean;


   -- @key[function] "+"   (Right : Float) @key[return] Float;
   -- @key[function] "-"   (Right : Float) @key[return] Float;
   -- @key[function] "@key[abs]" (Right : Float) @key[return] Float;


   -- @key[function] "+"   (Left, Right : Float) @key[return] Float;
   -- @key[function] "-"   (Left, Right : Float) @key[return] Float;
   -- @key[function] "*"   (Left, Right : Float) @key[return] Float;
   -- @key[function] "/"   (Left, Right : Float) @key[return] Float;


   -- @key[function] "**"  (Left : Float; Right : Integer'Base) @key[return] Float;


   --@ExamCom{ The specification of each operator for the type root_real, or for}
   --@ExamCom{ any additional predefined floating point type, is obtained by}
   --@ExamCom{ replacing Float by the name of the type in the specification of the}
   --@ExamCom{ corresponding operator of the type Float.}


@Keepnext   --@ExamCom{ In addition, the following operators are predefined for the root}
   --@ExamCom{ numeric types:}

   @key[function] "*" (Left : @VirtName{root_integer}; Right : @VirtName{root_real})
     @key[return] @VirtName{root_real};


   @key[function] "*" (Left : @VirtName{root_real};    Right : @VirtName{root_integer})
     @key[return] @VirtName{root_real};


   @key[function] "/" (Left : @VirtName{root_real};    Right : @VirtName{root_integer})
     @key[return] @VirtName{root_real};


   --@ExamCom{ The type universal_fixed is predefined.}
   --@ExamCom{ The only multiplying operators defined between}
   --@ExamCom{ fixed point types are}

   @key[function] "*" (Left : @VirtName[universal_fixed]; Right : @VirtName[universal_fixed])
     @key[return] @VirtName[universal_fixed];

   @key[function] "/" (Left : @VirtName[universal_fixed]; Right : @VirtName[universal_fixed])
     @key[return] @VirtName[universal_fixed];

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00230-01]}
@ChgAdded{Version=[2],Text=[   --@ExamCom{ The type universal_access is predefined.}
   --@ExamCom{ The following equality operators are predefined:}]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00230-01]}
@ChgAdded{Version=[2],Text=[   @key[function] "="  (Left, Right: @VirtName[universal_access]) @key[return] Boolean;
   @key[function] "/=" (Left, Right: @VirtName[universal_access]) @key[return] Boolean;]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00415-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0181-1],ARef=[AI05-0248-1]}
@tabclear()@tabset(P7, P14, P21, P28, P37, P44, P51, P58, P64)
@comment{blank line}
      --@ExamCom{ The declaration of type Character is based on the standard ISO 8859-1 character set.}
@comment{blank line}
      --@ExamCom{ There are no character literals corresponding to the positions for control characters.}
      --@ExamCom{ They are indicated in italics in this definition. See @refsecnum[Character Types].}
@comment[blank line]
   @key[type] @AdaTypeDefn{Character} @key[is]
     (@VirtName[nul],@\@VirtName[soh],@\@VirtName[stx],@\@VirtName[etx],@\@VirtName[eot],@\@VirtName[enq],@\@VirtName[ack],@\@VirtName[bel],@\--@ExamCom{0 (16#00#) .. 7 (16#07#)}
      @VirtName[bs],@\@VirtName[ht],@\@VirtName[lf],@\@VirtName[vt],@\@VirtName[ff],@\@VirtName[cr],@\@VirtName[so],@\@VirtName[si],@\--@ExamCom{8 (16#08#) .. 15 (16#0F#)}
@comment{blank line}
      @VirtName[dle],@\@VirtName[dc1],@\@VirtName[dc2],@\@VirtName[dc3],@\@VirtName[dc4],@\@VirtName[nak],@\@VirtName[syn],@\@VirtName[etb],@\--@ExamCom{16 (16#10#) .. 23 (16#17#)}
      @VirtName[can],@\@VirtName[em],@\@VirtName[sub],@\@VirtName[esc],@\@VirtName[fs],@\@VirtName[gs],@\@VirtName[rs],@\@VirtName[us],@\--@ExamCom{24 (16#18#) .. 31 (16#1F#)}
@comment{blank line}
      ' ',@\'!',@\'"',@\'#',@\'$',@\'%',@\'&',@\''',@\--@ExamCom{32 (16#20#) .. 39 (16#27#)}
      '(',@\')',@\'*',@\'+',@\',',@\'-',@\'.',@\'/',@\--@ExamCom{40 (16#28#) .. 47 (16#2F#)}
@comment{blank line}
      '0',@\'1',@\'2',@\'3',@\'4',@\'5',@\'6',@\'7',@\--@ExamCom{48 (16#30#) .. 55 (16#37#)}
      '8',@\'9',@\':',@\';',@\'<',@\'=',@\'>',@\'?',@\--@ExamCom{56 (16#38#) .. 63 (16#3F#)}
@comment{blank line}
      '@@',@\'A',@\'B',@\'C',@\'D',@\'E',@\'F',@\'G',@\--@ExamCom{64 (16#40#) .. 71 (16#47#)}
      'H',@\'I',@\'J',@\'K',@\'L',@\'M',@\'N',@\'O',@\--@ExamCom{72 (16#48#) .. 79 (16#4F#)}
@comment{blank line}
      'P',@\'Q',@\'R',@\'S',@\'T',@\'U',@\'V',@\'W',@\--@ExamCom{80 (16#50#) .. 87 (16#57#)}
      'X',@\'Y',@\'Z',@\'[',@\'\',@\']',@\'^',@\'_',@\--@ExamCom{88 (16#58#) .. 95 (16#5F#)}
@comment{blank line}
      '`',@\'a',@\'b',@\'c',@\'d',@\'e',@\'f',@\'g',@\--@ExamCom{96 (16#60#) .. 103 (16#67#)}
      'h',@\'i',@\'j',@\'k',@\'l',@\'m',@\'n',@\'o',@\--@ExamCom{104 (16#68#) .. 111 (16#6F#)}
@comment{blank line}
      'p',@\'q',@\'r',@\'s',@\'t',@\'u',@\'v',@\'w',@\--@ExamCom{112 (16#70#) .. 119 (16#77#)}
      'x',@\'y',@\'z',@\'{',@\'|',@\'}',@\'~',@\@VirtName[del],@\--@ExamCom{120 (16#78#) .. 127 (16#7F#)}
@Comment{Blank line}
      @VirtName[reserved_128],@\@VirtName[reserved_129],@\@VirtName[bph],@\@VirtName[nbh],@\@\@\--@ExamCom{128 (16#80#) .. 131 (16#83#)}
      @VirtName[reserved_132],@\@VirtName[nel],@\@VirtName[ssa],@\@VirtName[esa],@\@\@\@\--@ExamCom{132 (16#84#) .. 135 (16#87#)}
      @VirtName[hts],@\@VirtName[htj],@\@VirtName[vts],@\@VirtName[pld],@\@VirtName[plu],@\@VirtName[ri],@\@VirtName[ss2],@\@VirtName[ss3],@\--@ExamCom{136 (16#88#) .. 143 (16#8F#)}
@comment{blank line}
      @VirtName[dcs],@\@VirtName[pu1],@\@VirtName[pu2],@\@VirtName[sts],@\@VirtName[cch],@\@VirtName[mw],@\@VirtName[spa],@\@VirtName[epa],@\--@ExamCom{144 (16#90#) .. 151 (16#97#)}
      @VirtName[sos],@\@VirtName[reserved_153],@\@VirtName[sci],@\@VirtName[csi],@\@\@\@\--@VirtName{152 (16#98#) .. 155 (16#9B#)}
      @VirtName[st],@\@VirtName[osc],@\@VirtName[pm],@\@VirtName[apc],@\@\@\@\@\--@ExamCom{156 (16#9C#) .. 159 (16#9F#)}
@comment{blank line}
      ' ',@\'@latin1(161)',@\'@latin1(162)',@\'@latin1(163)',@\'@latin1(164)',@\'@latin1(165)',@\'@latin1(166)',@\'@latin1(167)',@\--@ExamCom{160 (16#A0#) .. 167 (16#A7#)}
      '@latin1(168)',@\'@latin1(169)',@\'@latin1(170)',@\'@latin1(171)',@Chg{Version=[3],New=[@\@\@\@\@\--@ExamCom{168 (16#A8#) .. 171 (16#AB#)}
      '@latin1(172)',@\@VirtName[soft_hyphen],@\'@latin1(174)',@\'@latin1(175)',@\@\@\@\--@ExamCom{172 (16#AC#) .. 175 (16#AF#)}],Old=[@\'@latin1(172)',@\'@latin1(173)',@\'@latin1(174)',@\'@latin1(175)',@\--@ExamCom{168 (16#A8#) .. 175 (16#AF#)}]}
@comment{blank line}
      '@latin1(176)',@\'@latin1(177)',@\'@latin1(178)',@\'@latin1(179)',@\'@latin1(180)',@\'@latin1(181)',@\'@latin1(182)',@\'@latin1(183)',@\--@ExamCom{176 (16#B0#) .. 183 (16#B7#)}
      '@latin1(184)',@\'@latin1(185)',@\'@latin1(186)',@\'@latin1(187)',@\'@latin1(188)',@\'@latin1(189)',@\'@latin1(190)',@\'@latin1(191)',@\--@ExamCom{184 (16#B8#) .. 191 (16#BF#)}
@comment{blank line}
      '@latin1(192)',@\'@latin1(193)',@\'@latin1(194)',@\'@latin1(195)',@\'@latin1(196)',@\'@latin1(197)',@\'@latin1(198)',@\'@latin1(199)',@\--@ExamCom{192 (16#C0#) .. 199 (16#C7#)}
      '@latin1(200)',@\'@latin1(201)',@\'@latin1(202)',@\'@latin1(203)',@\'@latin1(204)',@\'@latin1(205)',@\'@latin1(206)',@\'@latin1(207)',@\--@ExamCom{200 (16#C8#) .. 207 (16#CF#)}
@comment{blank line}
      '@latin1(208)',@\'@latin1(209)',@\'@latin1(210)',@\'@latin1(211)',@\'@latin1(212)',@\'@latin1(213)',@\'@latin1(214)',@\'@latin1(215)',@\--@ExamCom{208 (16#D0#) .. 215 (16#D7#)}
      '@latin1(216)',@\'@latin1(217)',@\'@latin1(218)',@\'@latin1(219)',@\'@latin1(220)',@\'@latin1(221)',@\'@latin1(222)',@\'@latin1(223)',@\--@ExamCom{216 (16#D8#) .. 223 (16#DF#)}
@comment{blank line}
      '@latin1(224)',@\'@latin1(225)',@\'@latin1(226)',@\'@latin1(227)',@\'@latin1(228)',@\'@latin1(229)',@\'@latin1(230)',@\'@latin1(231)',@\--@ExamCom{224 (16#E0#) .. 231 (16#E7#)}
      '@latin1(232)',@\'@latin1(233)',@\'@latin1(234)',@\'@latin1(235)',@\'@latin1(236)',@\'@latin1(237)',@\'@latin1(238)',@\'@latin1(239)',@\--@ExamCom{232 (16#E8#) .. 239 (16#EF#)}
@comment{blank line}
      '@latin1(240)',@\'@latin1(241)',@\'@latin1(242)',@\'@latin1(243)',@\'@latin1(244)',@\'@latin1(245)',@\'@latin1(246)',@\'@latin1(247)',@\--@ExamCom{240 (16#F0#) .. 247 (16#F7#)}
      '@latin1(248)',@\'@latin1(249)',@\'@latin1(250)',@\'@latin1(251)',@\'@latin1(252)',@\'@latin1(253)',@\'@latin1(254)',@\'@latin1(255)'@Chg{Version=[2],New=[);],Old=[,@\]}--@ExamCom{248 (16#F8#) .. 255 (16#FF#)}


   --@ExamCom{ The predefined operators for the type Character are the same as for}
   --@ExamCom{ any enumeration type.}
@Softpage@;

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00395-01]}@Comment{Odd missing paragraph number here}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0266-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0263-1],ARef=[AI12-0450-1]}
@Chg{Version=[2],New=[],Old=[@noparanum@;]}   --@ExamCom{ The declaration of type Wide_Character is based on the standard @Chg{Version=[2],New=[ISO/IEC],Old=[ISO]} 10646@Chg{Version=[2],New=[:@Chg{Version=[3],New=[@Chg{Version=[5],New=[2020],Old=[2011]}],Old=[2003]}],Old=[]} BMP character@Chg{Version=[2],New=[],Old=[ set.]}}
   --@ExamCom{ @Chg{Version=[2],New=[set. ],Old=[]}The first 256 positions have the same contents as type Character. See @refsecnum[Character types].}
@comment[blank line]
   @key[type] @AdaTypeDefn{Wide_Character} @key[is] (@VirtName[nul], @VirtName[soh] ... @VirtName[@Chg{Version=[2],New=[Hex_0000FFFE],Old=[FFFE]}], @VirtName[@Chg{Version=[2],New=[Hex_0000FFFF],Old=[FFFF]}]);

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0266-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0263-1],ARef=[AI12-0450-1]}
@ChgAdded{Version=[2],Text=[   --@ExamCom[ The declaration of type Wide_Wide_Character is based on the full]
   --@ExamCom[ ISO/IEC 10646:@Chg{Version=[3],New=[@Chg{Version=[5],New=[2020],Old=[2011]}],Old=[2003]} character set. The first 65536 positions have the]
   --@ExamCom[ same contents as type Wide_Character. See @refsecnum[Character types].]
@comment[blank line]
   @key[type] @AdaTypeDefn{Wide_Wide_Character} @key[is] (@VirtName[nul], @VirtName[soh] ... @VirtName[Hex_7FFFFFFE], @VirtName[Hex_7FFFFFFF]);
   @key[for] Wide_Wide_Character'Size @key[use] 32;]}

@ChgRef{Version=[2],Kind=[Added]}@Comment{Odd missing paragraph number here}
@Chg{Version=[2],New=[],Old=[@noparanum@;]}   @key[package] @AdaPackDefn{ASCII} @key[is] ... @key[end] ASCII;  --@ExamCom{Obsolescent; see @RefSecNum[ASCII]}
@Defn2{Term=[ASCII], Sec=(package physically nested within the declaration of Standard)}
@comment[blank line]

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
   --@ExamCom{ Predefined string types:}
@comment[blank line]
   @key[type] @AdaTypeDefn{String} @key[is] @key[array](Positive @key[range] <>) @key[of] Character@Chg{Version=[3],New=[
      @key[with] Pack],Old=[;
   @key[pragma] Pack(String)]};

@Keepnext   --@ExamCom{ The predefined operators for this type are as follows:}

   --     @key[function] "="  (Left, Right: String) @key[return] Boolean;
   --     @key[function] "/=" (Left, Right: String) @key[return] Boolean;@Softpage
   --     @key[function] "<"  (Left, Right: String) @key[return] Boolean;
   --     @key[function] "<=" (Left, Right: String) @key[return] Boolean;
   --     @key[function] ">"  (Left, Right: String) @key[return] Boolean;
   --     @key[function] ">=" (Left, Right: String) @key[return] Boolean;


   --     @key[function] "&" (Left: String;    Right: String)    @key[return] String;
   --     @key[function] "&" (Left: Character; Right: String)    @key[return] String;
   --     @key[function] "&" (Left: String;    Right: Character) @key[return] String;
   --     @key[function] "&" (Left: Character; Right: Character) @key[return] String;


@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
   @key[type] @AdaTypeDefn{Wide_String} @key[is] @key[array](Positive @key[range] <>) @key[of] Wide_Character@Chg{Version=[3],New=[
      @key[with] Pack],Old=[;
   @key[pragma] Pack(Wide_String)]};

   --@ExamCom{ The predefined operators for this type correspond to those for String.}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0229-1]}
@ChgAdded{Version=[2],Text=[   @key[type] @AdaTypeDefn{Wide_Wide_String} @key[is array] (Positive @key[range] <>)
      @key[of] Wide_Wide_Character@Chg{Version=[3],New=[
         @key[with] Pack],Old=[;
   @key[pragma] Pack (Wide_Wide_String)]};]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[   --@ExamCom[ The predefined operators for this type correspond to those for String.]]}


   @key[type] @AdaTypeDefn{Duration} @key[is] @key[delta] @VirtName{implementation-defined} @key[range] @VirtName{implementation-defined};

      --@ExamCom{ The predefined operators for the type Duration are the same as for}
      --@ExamCom{ any fixed point type.}


@Keepnext   --@ExamCom{ The predefined exceptions:}

   @AdaExcDefn{Constraint_Error}: @key[exception];
   @AdaExcDefn{Program_Error}   : @key[exception];
   @AdaExcDefn{Storage_Error}   : @key[exception];
   @AdaExcDefn{Tasking_Error}   : @key[exception];

@key[end] Standard;
@end{Example}

Standard has no private part.
@begin{Reason}
  This is important for portability. All library packages
  are children of Standard, and if Standard had a private part then
  it would be visible to all of them.
@end{reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
In each of the types Character@Chg{Version=[2],New=[,],Old=[ and]}
Wide_Character@Chg{Version=[2],New=[, and Wide_Wide_Character],Old=[]},
the character literals for the space character (position 32)
and the non-breaking
space character (position 160)
correspond to different values. Unless
indicated otherwise, each occurrence of
the character literal
' ' in this @IntlStdTitle refers to the space character.
Similarly, the character literals
for hyphen (position 45)
and soft hyphen (position 173) correspond to different values.
Unless indicated otherwise, each occurrence of
the character literal
'@en@;' in this @IntlStdTitle refers to the hyphen character.
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(package_body of Standard)}
Elaboration of the body of Standard has no effect.
@begin{Discussion}
Note that the language does not define where
this body appears in the environment @nt{declarative_part}
@em see @RefSec{Program Structure and Compilation Issues}.
@end{Discussion}
@end{RunTime}

@begin{ImplPerm}
 An implementation may provide additional predefined integer
 types and additional predefined floating point types.
 @Chg{Version=[5],New=[Some or],Old=[Not]} all of these types
 @Chg{Version=[5],New=[may be anonymous],Old=[need have names]}.
@begin{Honest}

  An implementation may add representation items to package Standard, for
  example to specify the internal codes of type Boolean,
  or the Small of type Duration.

@end{Honest}
@end{ImplPerm}

@begin{ImplAdvice}
If an implementation provides additional named predefined integer types,
then the names should end with @lquotes@;Integer@rquotes@; as in @lquotes@;Long_Integer@rquotes@;.
If an implementation provides additional named predefined
floating point types,
then the names should end with @lquotes@;Float@rquotes@; as in @lquotes@;Long_Float@rquotes@;.
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[If an implementation provides additional named predefined integer types,
then the names should end with @lquotes@;Integer@rquotes@;.
If an implementation provides additional named predefined
floating point types,
then the names should end with @lquotes@;Float@rquotes@;.]}]}
@end{ImplAdvice}

@begin{Notes}
Certain aspects of the predefined entities cannot be completely
described in the language itself. For example, although the
enumeration type Boolean can be written showing the two enumeration
literals False and True, the short-circuit control forms cannot be
expressed in the language.

As explained in
@ISODiff{NotISO=[@RefSecFull{Declarative Region}],ISOOnly=[@RefSecFullNum{Declarative Region}]}
and @ISODiff{NotISO=[@RefSecFull{The Compilation Process}],ISOOnly=[@RefSecFullNum{The Compilation Process}]},
the declarative region of the package Standard encloses every
library unit and consequently the main subprogram;
the declaration of every library unit is assumed to occur
within this declarative region.
@nt{Library_item}s
are assumed to be ordered in such a way that there are no forward
semantic dependences.
However, as explained in
@ISODiff{NotISO=[@RefSecFull{Visibility}],ISOOnly=[@RefSecFullNum{Visibility}]},
the only library units that are
visible within a given compilation unit are
the library units named by all @nt{with_clause}s that
apply to the given unit,
and moreover, within the declarative region of a given library unit,
that library unit itself.

If all @nt{block_statement}s of a program are named, then the name
of each program unit can always be written as an expanded name
starting with Standard (unless Standard is itself hidden).
The name of a library unit cannot be a homograph of a name (such as
Integer) that is already declared in Standard.

The exception Standard.Numeric_Error is defined in @RefSecNum{Numeric_Error}.
@begin{Discussion}
The declaration of Natural needs to appear between the declaration of
Integer and the (implicit) declaration of the "**" operator for Integer,
because a formal parameter of "**" is of subtype Natural.
This would be impossible in normal code, because the implicit
declarations for a type occur immediately after the type declaration,
with no possibility of intervening explicit declarations.
But we're in Standard, and Standard is somewhat magic anyway.

Using Natural as the subtype of the formal of "**" seems natural;
it would be silly to have a textual rule about Constraint_Error being
raised when there is a perfectly good subtype that means just that.
Furthermore, by not using Integer for that formal, it helps remind the
reader that the exponent remains Natural even when the left operand is
replaced with the derivative of Integer.
It doesn't logically imply that, but it's still useful as a reminder.

In any case, declaring these general-purpose subtypes of Integer close
to Integer seems more readable than declaring them much later.
@end{Discussion}
@end{Notes}

@begin{Extend83}
@Defn{extensions to Ada 83}
Package Standard is declared to be pure.
@begin{discussion}
The introduction of the types Wide_Character and Wide_String is not
an Ada 95 extension to Ada 83, since ISO WG9 has approved these as an
authorized extension of the original Ada 83 standard that is part
of that standard.
@end{discussion}
@end{Extend83}

@begin{DiffWord83}
Numeric_Error is made obsolescent.

The declarations of Natural and Positive are moved to just after the
declaration of Integer, so that "**" can refer to Natural without a
forward reference.
There's no real need to move Positive, too @em it just came along for
the ride.
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Types Wide_Wide_Character and Wide_Wide_String are new.]}
  @begin{Discussion}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[The inconsistencies associated with these
    types are documented in @RefSecNum{Character Types} and
    @RefSecNum{String Types}.]}
  @end{Discussion}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01]}
  @ChgAdded{Version=[2],Text=[Type @i<universal_access> and the equality
  operations for it are new.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0028],ARef=[AI95-00145-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected the parameter
  type for the Boolean operators declared in Standard.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0181-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Since soft_hyphen (position
  173) is defined to be nongraphic, gave it a name.]}
  @begin{Discussion}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[The inconsistencies associated with this
    change are documented in @RefSecNum{Scalar Types}.]}
  @end{Discussion}
@end{DiffWord2005}
