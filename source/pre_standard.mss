@comment{ $Source: e:\\cvsroot/ARM/Source/pre_standard.mss,v $ }
@comment{ $Revision: 1.33 $ $Date: 2006/10/18 00:25:28 $ $Author: Randy $ }
@Part(predefstandard, Root="ada.mss")

@Comment{$Date: 2006/10/18 00:25:28 $}

@RMNewPage@Comment{For printed RM Ada 2005}
@LabeledClause{The Package Standard}

@begin{Intro}
This clause outlines the specification of the package Standard
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
@RootLibUnit{Standard}@key[package] Standard @key[is]
   @key[pragma] Pure(Standard);

   @key[type] @AdaTypeDefn{Boolean} @key[is] (False, True);


@Keepnext   --@RI{ The predefined relational operators for this type are as follows:}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0028],ARef=[AI95-00145-01]}
   -- @key[function] "="   (Left, Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean;
   -- @key[function] "/="  (Left, Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean;
   -- @key[function] "<"   (Left, Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean;
   -- @key[function] "<="  (Left, Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean;
   -- @key[function] ">"   (Left, Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean;
   -- @key[function] ">="  (Left, Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean;


@Keepnext   --@RI{ The predefined logical operators and the predefined logical}
   --@RI{ negation operator are as follows:}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0028],ARef=[AI95-00145-01]}
   -- @key[function] "@key[and]" (Left, Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean@Chg{New=['Base],Old=[]};
   -- @key[function] "@key[or]"  (Left, Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean@Chg{New=['Base],Old=[]};
   -- @key[function] "@key[xor]" (Left, Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean@Chg{New=['Base],Old=[]};


@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0028],ARef=[AI95-00145-01]}
   -- @key[function] "@key[not]" (Right : Boolean@Chg{New=['Base],Old=[]}) @key[return] Boolean@Chg{New=['Base],Old=[]};


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
   --@RI{ The integer type root_integer @Chg{Version=[2],New=[and the],Old=[is predefined.]}}
   --@RI{ @Chg{Version=[2],New=[],Old=[The ]}corresponding universal type @Chg{Version=[2],New=[],Old=[is ]}universal_integer@Chg{Version=[2],New=[ are predefined],Old=[]}.}


   @key[type] @AdaTypeDefn{Integer} @key[is] @key{range} @RI{implementation-defined};


   @key[subtype] @AdaSubtypeDefn{Name=[Natural],Of=[Integer]}  @key[is] Integer @key[range] 0 .. Integer'Last;
   @key[subtype] @AdaSubtypeDefn{Name=[Positive],Of=[Integer]} @key[is] Integer @key[range] 1 .. Integer'Last;


@Keepnext   --@RI{ The predefined operators for type Integer are as follows:}

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


   --@RI{ The specification of each operator for the type}
   --@RI{ root_integer, or for any additional predefined integer}
   --@RI{ type, is obtained by replacing Integer by the name of the type}
   --@RI{ in the specification of the corresponding operator of the type}
   --@RI{ Integer. The right operand of the exponentiation operator}
   --@RI{ remains as subtype Natural.}


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
   --@RI{ The floating point type root_real @Chg{Version=[2],New=[and the],Old=[is predefined.]}}
   --@RI{ @Chg{Version=[2],New=[],Old=[The ]}corresponding universal type @Chg{Version=[2],New=[],Old=[is ]}universal_real@Chg{Version=[2],New=[ are predefined],Old=[]}.}


   @key[type] @AdaTypeDefn{Float} @key[is] @key{digits} @RI{implementation-defined};


@Keepnext   --@RI{ The predefined operators for this type are as follows:}

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


   --@RI{ The specification of each operator for the type root_real, or for}
   --@RI{ any additional predefined floating point type, is obtained by}
   --@RI{ replacing Float by the name of the type in the specification of the}
   --@RI{ corresponding operator of the type Float.}


@Keepnext   --@RI{ In addition, the following operators are predefined for the root}
   --@RI{ numeric types:}

   @key[function] "*" (Left : @RI{root_integer}; Right : @RI{root_real})
     @key[return] @RI{root_real};


   @key[function] "*" (Left : @RI{root_real};    Right : @RI{root_integer})
     @key[return] @RI{root_real};


   @key[function] "/" (Left : @RI{root_real};    Right : @RI{root_integer})
     @key[return] @RI{root_real};


   --@RI{ The type universal_fixed is predefined.}
   --@RI{ The only multiplying operators defined between}
   --@RI{ fixed point types are}

   @key[function] "*" (Left : @RI[universal_fixed]; Right : @RI[universal_fixed])
     @key[return] @RI[universal_fixed];

   @key[function] "/" (Left : @RI[universal_fixed]; Right : @RI[universal_fixed])
     @key[return] @RI[universal_fixed];

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00230-01]}
@ChgAdded{Version=[2],Text=[   --@RI{ The type universal_access is predefined.}
   --@RI{ The following equality operators are predefined:}]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00230-01]}
@ChgAdded{Version=[2],Text=[   @key[function] "="  (Left, Right: @RI[universal_access]) @key[return] Boolean;
   @key[function] "/=" (Left, Right: @RI[universal_access]) @key[return] Boolean;]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00415-01]}
@tabclear()@tabset(P7, P14, P21, P28, P37, P44, P51, P58, P64)
@comment{blank line}
      --@RI{ The declaration of type Character is based on the standard ISO 8859-1 character set.}
@comment{blank line}
      --@RI{ There are no character literals corresponding to the positions for control characters.}
      --@RI{ They are indicated in italics in this definition. See @refsecnum[Character Types].}
@comment[blank line]
   @key[type] @AdaTypeDefn{Character} @key[is]
     (@RI[nul],@\@RI[soh],@\@RI[stx],@\@RI[etx],@\@RI[eot],@\@RI[enq],@\@RI[ack],@\@RI[bel],@\--@RI{0 (16#00#) .. 7 (16#07#)}
      @RI[bs],@\@RI[ht],@\@RI[lf],@\@RI[vt],@\@RI[ff],@\@RI[cr],@\@RI[so],@\@RI[si],@\--@RI{8 (16#08#) .. 15 (16#0F#)}
@comment{blank line}
      @RI[dle],@\@RI[dc1],@\@RI[dc2],@\@RI[dc3],@\@RI[dc4],@\@RI[nak],@\@RI[syn],@\@RI[etb],@\--@RI{16 (16#10#) .. 23 (16#17#)}
      @RI[can],@\@RI[em],@\@RI[sub],@\@RI[esc],@\@RI[fs],@\@RI[gs],@\@RI[rs],@\@RI[us],@\--@RI{24 (16#18#) .. 31 (16#1F#)}
@comment{blank line}
      ' ',@\'!',@\'"',@\'#',@\'$',@\'%',@\'&',@\''',@\--@RI{32 (16#20#) .. 39 (16#27#)}
      '(',@\')',@\'*',@\'+',@\',',@\'-',@\'.',@\'/',@\--@RI{40 (16#28#) .. 47 (16#2F#)}
@comment{blank line}
      '0',@\'1',@\'2',@\'3',@\'4',@\'5',@\'6',@\'7',@\--@RI{48 (16#30#) .. 55 (16#37#)}
      '8',@\'9',@\':',@\';',@\'<',@\'=',@\'>',@\'?',@\--@RI{56 (16#38#) .. 63 (16#3F#)}
@comment{blank line}
      '@@',@\'A',@\'B',@\'C',@\'D',@\'E',@\'F',@\'G',@\--@RI{64 (16#40#) .. 71 (16#47#)}
      'H',@\'I',@\'J',@\'K',@\'L',@\'M',@\'N',@\'O',@\--@RI{72 (16#48#) .. 79 (16#4F#)}
@comment{blank line}
      'P',@\'Q',@\'R',@\'S',@\'T',@\'U',@\'V',@\'W',@\--@RI{80 (16#50#) .. 87 (16#57#)}
      'X',@\'Y',@\'Z',@\'[',@\'\',@\']',@\'^',@\'_',@\--@RI{88 (16#58#) .. 95 (16#5F#)}
@comment{blank line}
      '`',@\'a',@\'b',@\'c',@\'d',@\'e',@\'f',@\'g',@\--@RI{96 (16#60#) .. 103 (16#67#)}
      'h',@\'i',@\'j',@\'k',@\'l',@\'m',@\'n',@\'o',@\--@RI{104 (16#68#) .. 111 (16#6F#)}
@comment{blank line}
      'p',@\'q',@\'r',@\'s',@\'t',@\'u',@\'v',@\'w',@\--@RI{112 (16#70#) .. 119 (16#77#)}
      'x',@\'y',@\'z',@\'{',@\'|',@\'}',@\'~',@\@RI[del],@\--@RI{120 (16#78#) .. 127 (16#7F#)}
@Comment{Blank line}
      @RI[reserved_128],@\@RI[reserved_129],@\@RI[bph],@\@RI[nbh],@\@\@\--@RI{128 (16#80#) .. 131 (16#83#)}
      @RI[reserved_132],@\@RI[nel],@\@RI[ssa],@\@RI[esa],@\@\@\@\--@RI{132 (16#84#) .. 135 (16#87#)}
      @RI[hts],@\@RI[htj],@\@RI[vts],@\@RI[pld],@\@RI[plu],@\@RI[ri],@\@RI[ss2],@\@RI[ss3],@\--@RI{136 (16#88#) .. 143 (16#8F#)}
@comment{blank line}
      @RI[dcs],@\@RI[pu1],@\@RI[pu2],@\@RI[sts],@\@RI[cch],@\@RI[mw],@\@RI[spa],@\@RI[epa],@\--@RI{144 (16#90#) .. 151 (16#97#)}
      @RI[sos],@\@RI[reserved_153],@\@RI[sci],@\@RI[csi],@\@\@\@\--@RI{152 (16#98#) .. 155 (16#9B#)}
      @RI[st],@\@RI[osc],@\@RI[pm],@\@RI[apc],@\@\@\@\@\--@RI{156 (16#9C#) .. 159 (16#9F#)}
@comment{blank line}
      ' ',@\'@latin1(161)',@\'@latin1(162)',@\'@latin1(163)',@\'@latin1(164)',@\'@latin1(165)',@\'@latin1(166)',@\'@latin1(167)',@\--@RI{160 (16#A0#) .. 167 (16#A7#)}
      '@latin1(168)',@\'@latin1(169)',@\'@latin1(170)',@\'@latin1(171)',@\'@latin1(172)',@\'@latin1(173)',@\'@latin1(174)',@\'@latin1(175)',@\--@RI{168 (16#A8#) .. 175 (16#AF#)}
@comment{blank line}
      '@latin1(176)',@\'@latin1(177)',@\'@latin1(178)',@\'@latin1(179)',@\'@latin1(180)',@\'@latin1(181)',@\'@latin1(182)',@\'@latin1(183)',@\--@RI{176 (16#B0#) .. 183 (16#B7#)}
      '@latin1(184)',@\'@latin1(185)',@\'@latin1(186)',@\'@latin1(187)',@\'@latin1(188)',@\'@latin1(189)',@\'@latin1(190)',@\'@latin1(191)',@\--@RI{184 (16#B8#) .. 191 (16#BF#)}
@comment{blank line}
      '@latin1(192)',@\'@latin1(193)',@\'@latin1(194)',@\'@latin1(195)',@\'@latin1(196)',@\'@latin1(197)',@\'@latin1(198)',@\'@latin1(199)',@\--@RI{192 (16#C0#) .. 199 (16#C7#)}
      '@latin1(200)',@\'@latin1(201)',@\'@latin1(202)',@\'@latin1(203)',@\'@latin1(204)',@\'@latin1(205)',@\'@latin1(206)',@\'@latin1(207)',@\--@RI{200 (16#C8#) .. 207 (16#CF#)}
@comment{blank line}
      '@latin1(208)',@\'@latin1(209)',@\'@latin1(210)',@\'@latin1(211)',@\'@latin1(212)',@\'@latin1(213)',@\'@latin1(214)',@\'@latin1(215)',@\--@RI{208 (16#D0#) .. 215 (16#D7#)}
      '@latin1(216)',@\'@latin1(217)',@\'@latin1(218)',@\'@latin1(219)',@\'@latin1(220)',@\'@latin1(221)',@\'@latin1(222)',@\'@latin1(223)',@\--@RI{216 (16#D8#) .. 223 (16#DF#)}
@comment{blank line}
      '@latin1(224)',@\'@latin1(225)',@\'@latin1(226)',@\'@latin1(227)',@\'@latin1(228)',@\'@latin1(229)',@\'@latin1(230)',@\'@latin1(231)',@\--@RI{224 (16#E0#) .. 231 (16#E7#)}
      '@latin1(232)',@\'@latin1(233)',@\'@latin1(234)',@\'@latin1(235)',@\'@latin1(236)',@\'@latin1(237)',@\'@latin1(238)',@\'@latin1(239)',@\--@RI{232 (16#E8#) .. 239 (16#EF#)}
@comment{blank line}
      '@latin1(240)',@\'@latin1(241)',@\'@latin1(242)',@\'@latin1(243)',@\'@latin1(244)',@\'@latin1(245)',@\'@latin1(246)',@\'@latin1(247)',@\--@RI{240 (16#F0#) .. 247 (16#F7#)}
      '@latin1(248)',@\'@latin1(249)',@\'@latin1(250)',@\'@latin1(251)',@\'@latin1(252)',@\'@latin1(253)',@\'@latin1(254)',@\'@latin1(255)'@Chg{Version=[2],New=[);],Old=[,@\]}--@RI{248 (16#F8#) .. 255 (16#FF#)}


   --@RI{ The predefined operators for the type Character are the same as for}
   --@RI{ any enumeration type.}
@Softpage@;

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00395-01]}@Comment{Odd missing paragraph number here}
@Chg{Version=[2],New=[],Old=[@noparanum@;]}   --@RI{ The declaration of type Wide_Character is based on the standard @Chg{Version=[2],New=[ISO/IEC],Old=[ISO]} 10646@Chg{Version=[2],New=[:2003],Old=[]} BMP character@Chg{Version=[2],New=[],Old=[ set.]}}
   --@RI{ @Chg{Version=[2],New=[set. ],Old=[]}The first 256 positions have the same contents as type Character. See @refsecnum[Character types].}
@comment[blank line]
   @key[type] @AdaTypeDefn{Wide_Character} @key[is] (@RI[nul], @RI[soh] ... @RI[@Chg{Version=[2],New=[Hex_0000FFFE],Old=[FFFE]}], @RI[@Chg{Version=[2],New=[Hex_0000FFFF],Old=[FFFF]}]);

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Text=[   --@RI[ The declaration of type Wide_Wide_Character is based on the full]
   --@RI[ ISO/IEC 10646:2003 character set. The first 65536 positions have the]
   --@RI[ same contents as type Wide_Character. See @refsecnum[Character types].]
@comment[blank line]
   @key[type] @AdaTypeDefn{Wide_Wide_Character} @key[is] (@RI[nul], @RI[soh] ... @RI[Hex_7FFFFFFE], @RI[Hex_7FFFFFFF]);
   @key[for] Wide_Wide_Character'Size @key[use] 32;]}

@ChgRef{Version=[2],Kind=[Added]}@Comment{Odd missing paragraph number here}
@Chg{Version=[2],New=[],Old=[@noparanum@;]}   @key[package] @AdaPackDefn{ASCII} @key[is] ... @key[end] ASCII;  --@RI{Obsolescent; see @RefSecNum[ASCII]}
@Defn2{Term=[ASCII], Sec=(package physically nested within the declaration of Standard)}
@comment[blank line]

   --@RI{ Predefined string types:}
@comment[blank line]
   @key[type] @AdaTypeDefn{String} @key[is] @key[array](Positive @key[range] <>) @key[of] Character;
   @key[pragma] Pack(String);

@Keepnext   --@RI{ The predefined operators for this type are as follows:}

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


   @key[type] @AdaTypeDefn{Wide_String} @key[is] @key[array](Positive @key[range] <>) @key[of] Wide_Character;
   @key[pragma] Pack(Wide_String);

   --@RI{ The predefined operators for this type correspond to those for String.}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[   @key[type] @AdaTypeDefn{Wide_Wide_String} @key[is array] (Positive @key[range] <>)
     @key[of] Wide_Wide_Character;
   @key[pragma] Pack (Wide_Wide_String);]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[   --@RI[ The predefined operators for this type correspond to those for String.]]}


   @key[type] @AdaTypeDefn{Duration} @key[is] @key[delta] @RI{implementation-defined} @key[range] @RI{implementation-defined};

      --@RI{ The predefined operators for the type Duration are the same as for}
      --@RI{ any fixed point type.}


@Keepnext   --@RI{ The predefined exceptions:}

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
' ' in this International Standard
refers to the space character.
Similarly, the character literals
for hyphen (position 45)
and soft hyphen (position 173) correspond to different values.
Unless indicated otherwise, each occurrence of
the character literal
'@en@;' in this International Standard
refers to the hyphen character.
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
  Not all of these types need have names.
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

As explained in @RefSec{Declarative Region}
and @RefSec{The Compilation Process},
the declarative region of the package Standard encloses every
library unit and consequently the main subprogram;
the declaration of every library unit is assumed to occur
within this declarative region.
@nt{Library_item}s
are assumed to be ordered in such a way that there are no forward
semantic dependences.
However, as explained in @RefSec{Visibility}, the only library units that are
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
  type for the Boolean operators declared in Standard..]}
@end{DiffWord95}
