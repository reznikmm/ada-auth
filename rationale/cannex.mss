@Part(xxx, Root="rat.msm")

@comment($Source: e:\\cvsroot/ARM/Rationale/cannex.mss,v $)
@comment($Revision: 1.2 $ $Date: 2006/02/09 07:54:21 $)

@LabeledClause{Summary table}

This paper concludes with a table showing at a glance the
various facilities in the six main containers.

@leading@;In order to save space the following abbreviations are used in the
table:

@Comment{@begin[Example] --Original version:
@Tabset[P7,P35,P42]
T@\container type eg Map@\H_T@\Hash_Type
C: T@\Container: container type@\I_T@\Index_Type
P: C@\Position: Cursor@\K_T@\Key_Type
L, R@\Left, Right@\Ex_Index@\Extended_Index
C_T@\Count_Type@\B@\Boolean
E_T@\Element_Type
@end[Example]}
@table{Columns=[4],Caption=[],Headers=[],
Body=[
T@\container type eg Map@\H_T@\Hash_Type
C: T@\Container: container type@\I_T@\Index_Type
P: C@\Position: Cursor@\K_T@\Key_Type
L, R@\Left, Right@\Ex_Index@\Extended_Index
C_T@\Count_Type@\B@\Boolean@last
E_T@\Element_Type@\@\]}

also Index @en means that another subprogram exists with similar parameters
except that the first parameters are of type Vector and Index_Type
(or Extended_Index) rather than those involving cursors.

also Key and also Element similarly apply to maps and sets respectively.

@table{Columns=[7],Caption=[],
Headers=[@\vectors@\lists@\hashed maps@\ordered maps@\hashed sets@\ordered sets],
Body=[@s{@shrink{@key[generic]}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{  @key[type] Index_Type @key[is range] <>;}}@\@s{@shrink{Y}}@\@\@\@\@\
@s{@shrink{  @key[type] Key_Type @key[is private];}}@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@\
@s{@shrink{  @key[type] Element_Type @key[is private];}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{  @key[with function] Hash( ... ) @key[return] Hash_Type;}}@\@\@\@s{@shrink{on Key}}@\@\@s{@shrink{on Element}}@\
@s{@shrink{    with
function Equivalent_...(L, R: ...) return Boolean;}}@\ @\ \pard
\s43\ql
{@\ }
on Key@\
{@\ }
on Element@\
{@\ }\pard\plain \ql


















@s{@shrink{    with
function "<" (L, R: ... ) return Boolean is <>;}}@\ @\

{@\ }
{@\ on Key@\ }
{@\ }
on Element@\ \pard\plain \ql


















@s{@shrink{    with
function "=" (L, R: E_T) return B is <>;}}@\ @s{@shrink{Y}}@\

@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ @b[package]
Ada.Containers.... is@\ Vectors}}@\
Doubly_
Linked_
Lists@\
Hashed_
Maps@\
Ordered_
Maps@\ Hashed_
Sets@\
Ordered_
Sets@\ \pard\plain \ql


















@s{@shrink{ pragma
Preelaborate( ... );}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ function
Equivalent_...(L, R: ...) return Boolean;}}@\ @\

{@\ }
{@\ on Key@\ }
{@\ }
on Element@\ \pard\plain \ql


















@s{@shrink{ subtype
Extended_Index ...

No_Index: constant Ex_Ind := Ex_Ind'First;}}@\
@s{@shrink{Y}}@\
{@\ @\ }
{@\ }
{@\ @\ }\pard\plain \ql


















@s{@shrink{ type T
is tagged private;
pragma Preelaborable_Initialization(T);}}@\ Vector@\

List@\
Map@\ Map@\
Set@\
Set@\ \pard\plain \ql


















@s{@shrink{ type Cursor
is private;
pragma Preelaborable_Initialization(Cursor);}}@\ @s{@shrink{Y}}@\

@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ Empty_T:
constant T;@\ Vector}}@\
List@\
Map@\ Map@\
Set@\
Set@\ \pard\plain \ql


















@s{@shrink{ No_Element:
constant Cursor;}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ function
"=" (Left, Right: T) return Boolean;}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ function
Equivalent_Sets(L, R: Set) return Boolean;

function To_Set(New_Item: E_T) return Set;}}@\
{@\ }
{@\ @\ }
{@\ }
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ function
To_Vector(Length: C_T) return Vector;

function To_Vector(New_Item: E_T;
                               Length: C_T) return Vector;}}@\ \pard
\s43\ql
@s{@shrink{Y}}@\
{@\ @\ }
{@\ }
{@\ @\ }\pard\plain \ql


















@s{@shrink{ function
"&" (L, R: Vector) return Vector;

function "&" (L: Vector; R: E_T) return Vector;


function "&" (L: E_T; R: Vector) return Vector;


function "&" (L, R: E_T) return Vector;}}@\
@s{@shrink{Y}}@\
{@\ @\ }
{@\ }
{@\ @\ }\pard\plain \ql


















@s{@shrink{ function
Capacity(C: T) return C_T;

procedure Reserve_Capacity(C: T; Capacity: C_T);}}@\

@s{@shrink{Y}}@\
{@\ @s{@shrink{Y}}@\ }
{@\ }
@s{@shrink{Y}}@\ @\ \pard\plain \ql


















@s{@shrink{ function
Length(C: T) return Count_Type;}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ procedure
Set_Length(C: in out T; Length: in C_T);}}@\ @s{@shrink{Y}}@\

{@\ }
{@\ @\ }
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ function
Is_Empty(C: T) return B;

procedure Clear(C: in out T);}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ function
To_Cursor(C: Vector; Index: Ex_Ind)
                                return Cursor;

function To_Index(P: C) return Ex_Ind;}}@\
@s{@shrink{Y}}@\
{@\ @\ }
{@\ }
{@\ @\ }\pard\plain \ql


















@s{@shrink{ function
Key(P: C) return K_T;}}@\ @\
{@\ }
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ function
Element(P: C) return E_T;}}@\ Y


also Index@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ procedure
Replace_Element(C: in out T; P: C;
                                              New_Item: E_T);}}@\
Y


also Index@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ procedure
Query_Element(P: C;
   Process: not null acc proc( ... ) );}}@\ in Element


also Index@\
in Element@\ in Key,
in Element@\
in Key,
in Element@\
in Element@\ in Element@\ \pard\plain \ql


















@s{@shrink{ procedure
Update_Element(C: in out T; P: C;
   Process: not null acc proc( ... ) );}}@\ in out Elem


also Index@\
in out Elem@\
in Key,
in out Elem@\ in Key,
in out Elem@\
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ procedure
Move(Target, Source: in out T);}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ procedure
Insert(C: in out Vector; Before: Ex_Ind;
                           New_Item: Vector);

procedure Insert(C: in out Vector; Before: Cursor;
                           New_Item: Vector);


procedure Insert(C: in out Vector; Before: Cursor;
                       New_Item: Vector; Position: out Cursor);}}@\

@s{@shrink{Y}}@\ @\
{@\ }
{@\ @\ }
{@\ }\pard\plain \ql


















@s{@shrink{ procedure
Insert(C: in out T; Before: C;
                           New_Item: E_T; Count: C_T := 1);}}@\ Y


also Index@\
@s{@shrink{Y}}@\
{@\ @\ }
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ procedure
Insert(C: in out T; Before: C;
                           New_Item: E_T; Position: out Cursor;
                           Count: C_T := 1);}}@\ @s{@shrink{Y}}@\

@s{@shrink{Y}}@\
{@\ @\ }
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ procedure
Insert(C: in out T; Before: C;
                          Position: out Cursor; Count: C_T := 1);

element has default value}}@\
Y


also Index@\
@s{@shrink{Y}}@\
{@\ @\ }
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ procedure
Insert(C: in out T; Key: K_T;
                           New_Item: E_T; Position: out Cursor;
                           Inserted: out B);}}@\ @\

{@\ }
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
Y (no key)@\
Y (no key)@\ \pard\plain \ql


















@s{@shrink{ procedure
Insert(C: in out T; Key: K_T;
                           Position: out Cursor; Inserted: out B);

element has default value}}@\
{@\ }
{@\ @s{@shrink{Y}}@\ }
@s{@shrink{Y}}@\
{@\ @\ }\pard\plain \ql


















@s{@shrink{ procedure
Insert(C: in out T; Key: K_T;
                           New_Item: E_T);}}@\ @\

{@\ }
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
Y (no key)@\
Y (no key)@\ \pard\plain \ql


















@s{@shrink{ procedure
Prepend(C: in out Vector;
                                New_Item: Vector);}}@\ @s{@shrink{Y}}@\ \pard
\s43\ql
{@\ }
{@\ @\ }
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ procedure
Prepend(C: in out T;
                                New_Item: E_T; Count: C_T := 1);}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\
{@\ @\ }
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ procedure
Append(C: in out Vector;
                               New_Item: Vector);}}@\ @s{@shrink{Y}}@\ \pard
\s43\ql
{@\ }
{@\ @\ }
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ procedure
Append(C: in out T;
                               New_Item: E_T; Count: C_T := 1);}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\
{@\ @\ }
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ procedure
Insert_Space(C: in out V; Before: Cursor;
                          Position: out Cursor; Count: C_T := 1);}}@\
Y


also Index@\
{@\ }
{@\ @\ }
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ procedure
Include(C: in out T;
                              Key: Key_Type; New_Item: E_T);}}@\
@\
{@\ }
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
Y (no key)@\
Y (no key)@\ \pard\plain \ql


















@s{@shrink{ procedure
Replace(C: in out T;
                              Key: Key_Type; New_Item: E_T);}}@\
@\
{@\ }
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
Y (no key)@\
Y (no key)@\ \pard\plain \ql


















@s{@shrink{ procedure
Exclude(C: in out T;
                               Key: Key_Type);}}@\ @\

{@\ }
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
Y (Item not key)@\
Y (item not key) @\ \pard\plain \ql


















@s{@shrink{ procedure
Delete(C: in out T; P: in out C;
                             Count: C_T := 1);}}@\ Y


also Index@\
@s{@shrink{Y}}@\
Y (no count)

also Key@\
Y (no count)


also Key@\
Y (no count)


also Element@\
Y (no count)


also Element@\ \pard\plain \ql


















@s{@shrink{ procedure
Delete_First(C: in out T; Count: C_T := 1);

procedure Delete_Last(C: in out T; Count: C_T := 1);}}@\

@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ @\
Y (no count)@\
{@\ Y (no count)@\ }\pard\plain \ql


















@s{@shrink{ procedure
Reverse_Elements(C: in out T);}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
{@\ @\ }
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ procedure
Swap(C: in out T; I, J: Cursor);}}@\ Y


also Index@\
@s{@shrink{Y}}@\
{@\ @\ }
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ procedure
Swap_Links(C: in out List; I, J: Cursor);}}@\ @\

@s{@shrink{Y}}@\
{@\ @\ }
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ procedure
Splice(Target: in out List; Before: Cursor;
                            Source: in out List);

procedure Splice(Target: in out List; Before: Cursor;
                   Source: in out List; Position: in out Cursor);


procedure Splice(Container: in out List; Before: Cursor;
                             Position: in out Cursor);}}@\ \pard
\s43\ql
{@\ }
@s{@shrink{Y}}@\ @\
{@\ }
{@\ @\ }\pard\plain \ql


















@s{@shrink{ procedure
Union(Target: in out Set; Source: Set);

function Union(L, R: Set) return Set;


function "or" (L, R: Set) return Set renames Union;}}@\

{@\ }
{@\ @\ }
{@\ }
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ procedure
Intersection(Target: in out Set;
                                     Source: Set);

function Intersection(L, R: Set) return Set;


function "and" (L, R: Set) return Set
                        renames Intersection;}}@\
{@\ }
{@\ @\ }
{@\ }
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ procedure
Difference(Target: in out Set; Source: Set);

function Difference(L, R: Set) return Set;


function "@en" (L, R: Set) return Set renames Difference;}}@\ \pard
\s43\ql
{@\ @\ }
{@\ }
{@\ @s{@shrink{Y}}@\ }
@s{@shrink{Y}}@\ \pard\plain \ql







\pard\plain \s43\ql

@s{@shrink{ procedure
Symmetric_Difference(Target: in out Set;
                                                      Source: Set);

function Symmetric_Difference (L, R: Set) return Set;


function "xor" (L, R: Set) return Set
                       renames Symmetric_Difference;}}@\

{@\ @\ }
{@\ }
{@\ @s{@shrink{Y}}@\ }
@s{@shrink{Y}}@\ \pard\plain \ql
 {\trowd \trgaph108\trrh518\trleft-108\trkeep\trbrdrt\brdrs\brdrw10















\pard\plain \s43\ql

@s{@shrink{ function
Overlap(L, R: Set) return Boolean;

function Is_Subset(Subset: Set; Of_Set: Set) return B;}}@\ \pard
\s43\ql
{@\ }
{@\ @\ }
{@\ }
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\ \pard\plain \ql
 {\trowd \trgaph108\trrh374\trleft-108\trkeep\trbrdrt\brdrs\brdrw10























\pard\plain \s43\ql

@s{@shrink{ function
First_Index(C: T) return Index_Type;}}@\ @s{@shrink{Y}}@\
{@\ }
{@\ @\ }
{@\ }
{@\ }\pard\plain \ql













@s{@shrink{ function
First(C: T) return Cursor;}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ function
First_Element(C: T) return Element_Type;}}@\ @s{@shrink{Y}}@\

@s{@shrink{Y}}@\
{@\ @s{@shrink{Y}}@\ }
{@\ }
@s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ function
First_Key(C: T) return Key_Type;}}@\ @\
{@\ }
{@\ @s{@shrink{Y}}@\ }
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ function
Last_Index(C: T) return Ex_Ind;}}@\ @s{@shrink{Y}}@\
{@\ }
{@\ @\ }
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ function
Last(C: T) return Cursor;}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
{@\ @s{@shrink{Y}}@\ }
{@\ }
@s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ function
Last_Element(C: T) return Element_Type;}}@\ @s{@shrink{Y}}@\

@s{@shrink{Y}}@\
{@\ @s{@shrink{Y}}@\ }
{@\ }
@s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ function
Last_Key(C: T) return Key_Type;}}@\ @\
{@\ }
{@\ @s{@shrink{Y}}@\ }
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ function
Next(P: C) return Cursor;

procedure Next(P: in out C);}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ function
Previous(P: C) return Cursor;

procedure Previous(P: in out C);}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ @\
@s{@shrink{Y}}@\
{@\ @s{@shrink{Y}}@\ }\pard\plain \ql


















@s{@shrink{ function
Find_Index(C: T; Item: E_T;
                            Index: I_T := I_T'First) return Ex_Ind;}}@\
@s{@shrink{Y}}@\
{@\ }
{@\ @\ }
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ function
Find(C: T; ... ; P: C := No_Element)
                      return Cursor;}}@\ Element@\

Element@\
Key (no position)@\ Key (no position)@\
Element (no position)@\
Element (no position)@\ \pard\plain \ql


















@s{@shrink{ function
Element(C: T; Key: K_T) return E_T;}}@\ @\
{@\ }
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ function
Reverse_Find_Index(C: T; Item: E_T;
                            Index: I_T := I_T'First) return Ex_Ind;}}@\
@s{@shrink{Y}}@\
{@\ }
{@\ @\ }
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ function
Reverse_Find(C: T; ... ; P: C := No_Element)
                                     return Cursor;}}@\ Element@\

Element@\
{@\ @\ }
{@\ }
{@\ }\pard\plain \ql


















@s{@shrink{ function
Floor(C: T; ...) return Cursor;

function Ceiling(C: T; ...) return Cursor;}}@\
{@\ }
{@\ @\ }
Key: K_T@\
{@\ }
Item: E_T@\ \pard\plain \ql


















@s{@shrink{ function
Contains(C: T; ...) return Boolean;@\ Element}}@\

Element@\
Key @\ Key @\
Element @\
Element@\ \pard\plain \ql


















@s{@shrink{ function
Has_Element(P: C) return Boolean;}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ function
Equivalent_... (L, R: Cursor) return Boolean;

function Equivalent_... (L: Cursor; R:...) return Boolean};


function Equivalent_... (L:...; R: Cursor) return Boolean};}}@\
{@\ }
{@\ Keys @\ }
{@\ }
Elements @\ @\ \pard\plain \ql


















@s{@shrink{ function
"<" (L, R: Cursor) return Boolean;

function ">" (L, R: Cursor) return Boolean;


function "<" (L, Cursor; R: ...) return Boolean;


function ">" (L, Cursor; R: ...) return Boolean;


function "<" (L:...; R: Cursor) return Boolean;


function ">" (L:...; R: Cursor) return Boolean;}}@\ @\

{@\ }
{@\ Key @\ }
{@\ }
Element @\ \pard\plain \ql


















@s{@shrink{ procedure
Iterate(C: in T;
   Process: not null acc proc (P: C) );}}@\ @s{@shrink{Y}}@\

@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ \pard\plain \ql


















@s{@shrink{ procedure
Reverse_Iterate(C: in T;
   Process: not null acc proc (P: C) );}}@\ @s{@shrink{Y}}@\

@s{@shrink{Y}}@\
{@\ @s{@shrink{Y}}@\ }
{@\ }
@s{@shrink{Y}}@\ \pard\plain \ql

















@s{@shrink{ @b[generic]
   with function "<" (Left, Right: E_T) return B is <>;
@b[package] Generic_Sorting is
   function Is_Sorted(C: T) return Boolean;
   procedure Sort(C: in out T);
   procedure Merge(Target, Source: in out T);
@b[end] Generic_Sorting;}}@\ @s{@shrink{Y}}@\
@s{@shrink{Y}}@\
{@\ @\ }
{@\ }
{@\ }\pard\plain \ql













@s{@shrink{ @b[generic]
   type Key_Type (<>) is private;}}@\ @\
{@\ }
{@\ @\ }
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ \pard\plain \ql













@s{@shrink{    with
function Key(Element: E_T) return Key_Type;}}@\ @\

{@\ @\ }
{@\ }
@s{@shrink{Y}}@\ @s{@shrink{Y}}@\ \pard\plain \ql












@s{@shrink{    with
function Hash(Key: K_T) return Hash_Type;}}@\ @\

{@\ }
{@\ @\ }
@s{@shrink{Y}}@\
{@\ }\pard\plain \ql











@s{@shrink{    with
function Equivalent_Keys (L, R: Key_Type)
                                                     return Boolean;}}@b[@\
]{@\ }
{@\ @\ }
{@\ }
@s{@shrink{Y}}@\ @\ \pard\plain \ql
















@s{@shrink{    with
function "<" (L, R: Key_Type) return B is <>;}}@\ @\

{@\ }
{@\ @\ }
{@\ }
@s{@shrink{Y}}@\ \pard\plain \ql















@s{@shrink{ @b[package]
Generic_Keys is}}@\ @\
{@\ }
{@\ @\ }
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ \pard\plain \ql














@s{@shrink{ function
Equivalent_Keys(L, R: Key_Type) return B;}} @\ @\

{@\ }
{@\ @\ }
{@\ }
@s{@shrink{Y}}@\ \pard\plain \ql













@s{@shrink{ function
Key(P: C) return Key_Type;}}@\ @\
{@\ }
{@\ @\ }
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ \pard\plain \ql













@s{@shrink{ function
Element(C: T; Key: K_T) return Element_T;}}@\ @\

{@\ }
{@\ @\ }
@s{@shrink{Y}}@\
@s{@shrink{Y}}@\ \pard\plain \ql
@s{@shrink{@key[procedure] Replace(C: in out T; Key: Key_Type;@*    New_Item: E_T);@*@*@key[procedure] Exclude(C: @key[in out] T; Key: Key_Type);@*@*@key[procedure] Delete(C: @key[in out] T; Key: Key_Type);}}@\@\@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[function] Find(C: T; Key: K_T) @key[return] Cursor;}}@\@\@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[function] Floor(C: T; Key: K_T) @key[return] Cursor;@*@*@key[function] Ceiling(C: T; Key: K_T) @key[return] Cursor;}}@\ @\ @\ @\ @\ @\@s{@shrink{Y}}
@s{@shrink{@key[function] Contains(C: T; Key: K_T) @key[return] Boolean;}}@\ @\ @\ @\ @\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[procedure]@*Update_Element_Preserving_Key@*   (C: in out T; P: C;@*   Process: @key[not null acc proc] (Element: @key[in out] E_T) );}}@\ @\ @\ @\ @\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[end] Generic_Keys;}}@\@\@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}@last
@s{@shrink{@key[private]@*  ... -- @i[not specified by the language]@*@key[end] Ada.Containers....;}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}]}

