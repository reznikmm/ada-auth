@Part(xxx, Root="rat.msm")

@comment($Source: e:\\cvsroot/ARM/Rationale/cannex.mss,v $)
@comment($Revision: 1.3 $ $Date: 2006/02/10 07:11:29 $)

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
@table{Columns=[4],Alignment=[AllLeft],FirstColWidth=[1],NoBreak=[T],
Caption=[],
Headers=[Abbrv.@\Meaning@\Abbrv.@\Meaning],
Body=[T@\container type eg Map@\H_T@\Hash_Type
C: T@\Container: container type@\I_T@\Index_Type
P: C@\Position: Cursor@\K_T@\Key_Type
L, R@\Left, Right@\Ex_Index@\Extended_Index
C_T@\Count_Type@\B@\Boolean@last
E_T@\Element_Type@\@\]}

also Index @en means that another subprogram exists with similar parameters
except that the first parameters are of type Vector and Index_Type
(or Extended_Index) rather than those involving cursors.

also Key and also Element similarly apply to maps and sets respectively.

@table{Columns=[7],Alignment=[CenterExceptFirst],FirstColWidth=[4],NoBreak=[F],
Caption=[],
Headers=[@\@s{@shrink{vectors}}@\@s{@shrink{lists}}@\@s{@shrink{hashed maps}}@\@s{@shrink{ordered maps}}@\@s{@shrink{hashed sets}}@\@s{@shrink{ordered sets}}],
Body=[@s{@shrink{@key[generic]}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@ @ @key[type] Index_Type @key[is range] <>;}}@\@s{@shrink{Y}}@\@\@\@\@\
@s{@shrink{@ @ @key[type] Key_Type @key[is private];}}@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@\
@s{@shrink{@ @ @key[type] Element_Type @key[is private];}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@ @ @key[with function] Hash( ... ) @key[return] Hash_Type;}}@\@\@\@s{@shrink{on Key}}@\@\@s{@shrink{on Element}}@\
@s{@shrink{@ @ @key[with function] Equivalent_...(L, R: ...) @key[return] Boolean;}}@\@\@\@s{@shrink{on Key}}@\@\@s{@shrink{on Element}}@\
@s{@shrink{@ @ @key[with function] "<" (L, R: ... ) @key[return] Boolean is <>;}}@\@\@\@\@s{@shrink{on Key}}@\@\@s{@shrink{on Element}}
@s{@shrink{@ @ @key[with function] "=" (L, R: E_T) @key[return] B is <>;}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[package] Ada.Containers.... @key[is]}}@\@s{@shrink{Vectors}}@\@s{@shrink{Doubly_@*Linked_@*Lists}}@\@s{@shrink{Hashed_@*Maps}}@\@s{@shrink{Ordered_@*Maps}}@\@s{@shrink{Hashed_@*Sets}}@\@s{@shrink{Ordered_@*Sets}}
@s{@shrink{@key[pragma] Preelaborate( ... );}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\ @s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[function] Equivalent_...(L, R: ...) @key[return] Boolean;}}@\@\@\@\@s{@shrink{on Key}}@\@\@s{@shrink{on Element}}
@s{@shrink{@key[subtype] Extended_Index ...@*No_Index: @key[constant] Ex_Ind := Ex_Ind'First;}}@\@s{@shrink{Y}}@\@\@\@\@\
@s{@shrink{@key[type] T @key[is tagged private];@*@key[pragma] Preelaborable_Initialization(T);}}@\@s{@shrink{Vector}}@\@s{@shrink{List}}@\@s{@shrink{Map}}@\@s{@shrink{Map}}@\@s{@shrink{Set}}@\@s{@shrink{Set}}
@s{@shrink{@key[type] Cursor @key[is private];@*@key[pragma] Preelaborable_Initialization(Cursor);}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{Empty_T: @key[constant] T;}}@\@s{@shrink{Vector}}@\@s{@shrink{List}}@\@s{@shrink{Map}}@\@s{@shrink{Map}}@\@s{@shrink{Set}}@\@s{@shrink{Set}}
@s{@shrink{No_Element: @key[constant] Cursor;}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\ @s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[function] "=" (Left, Right: T) @key[return] Boolean;}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[function] Equivalent_Sets(L, R: Set) @key[return] Boolean;@*@key[function] To_Set(New_Item: E_T) @key[return] Set;}}@\@\@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[function] To_Vector(Length: C_T) @key[return] Vector;@*@key[function] To_Vector(@*@ @ @ New_Item: E_T;@*@ @ @ Length: C_T) @key[return] Vector;}}@\@s{@shrink{Y}}@\@\@\@\@\
@s{@shrink{@key[function] "&" (L, R: Vector) @key[return] Vector;@*@key[function] "&" (L: Vector; R: E_T) @key[return] Vector;@*@key[function] "&" (L: E_T; R: Vector) @key[return] Vector;@*@key[function] "&" (L, R: E_T) @key[return] Vector;}}@\@s{@shrink{Y}}@\@\@\@\@\
@s{@shrink{@key[function] Capacity(C: T) @key[return] C_T;@*@key[procedure] Reserve_Capacity(C: T; Capacity: C_T);}}@\@s{@shrink{Y}}@\@\@s{@shrink{Y}}@\@\@s{@shrink{Y}}@\
@s{@shrink{@key[function] Length(C: T) @key[return] Count_Type;}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\ @s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[procedure] Set_Length(C: @key[in out] T; Length: @key[in] C_T);}}@\ @s{@shrink{Y}}@\@\@\@\@\
@s{@shrink{@key[function] Is_Empty(C: T) return B;@*@key[procedure] Clear(C: @key[in out] T);}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\ @s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[function] To_Cursor(C: Vector; Index: Ex_Ind)@* @ @ @ @ @ @ @key[return] Cursor;@*@key[function] To_Index(P: C) @key[return] Ex_Ind;}}@\@s{@shrink{Y}}@\@\@\@\@\
@s{@shrink{@key[function] Key(P: C) @key[return] K_T;}}@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@\
@s{@shrink{@key[function] Element(P: C) @key[return] E_T;}}@\@s{@shrink{Y@*also Index}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\ @s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[procedure] Replace_Element(@*@ @ @ C: in out T; P: C;@*@ @ @ New_Item: E_T);}}@\@s{@shrink{Y@*also Index}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\ @s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[procedure] Query_Element(P: C;@*@ @ @ Process: @key[not null acc proc]( ... ) );}}@\@s{@shrink{@key[in] Element@*also Index}}@\@s{@shrink{@key[in] Element}}@\@s{@shrink{@key[in] Key,@*@key[in] Element}}@\@s{@shrink{@key[in] Key,@*@key[in] Element}}@\@s{@shrink{@key[in] Element}}@\@s{@shrink{@key[in] Element}}
@s{@shrink{@key[procedure] Update_Element(C: @key[in out] T; P: C;@*@ @ @ Process: @key[not null acc proc]( ... ) );}}@\@s{@shrink{@key[in out] Elem@*also Index}}@\@s{@shrink{@key[in out] Elem}}@\@s{@shrink{@key[in] Key,@*@key[in out] Elem}}@\@s{@shrink{@key[in] Key,@*@key[in out] Elem}}@\@\
@s{@shrink{@key[procedure] Move(Target, Source: @key[in out] T);}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[procedure] Insert(@*@ @ @ C: @key[in out] Vector; Before: Ex_Ind;@*@ @ @ New_Item: Vector);@*@key[procedure] Insert(@*@ @ @ C: @key[in out] Vector; Before: Cursor;@*@ @ @ New_Item: Vector);@*@key[procedure] Insert(@*@ @ @ C: @key[in out] Vector; Before: Cursor;@*@ @ @ New_Item: Vector; Position: @key[out] Cursor);}}@\@s{@shrink{Y}}@\@\@\@\@\
@s{@shrink{@key[procedure] Insert(@*@ @ @ C: @key[in out] T; Before: C;@*@ @ @ New_Item: E_T; Count: C_T := 1);}}@\@s{@shrink{Y@*also Index}}@\@s{@shrink{Y}}@\@\@\@\
@s{@shrink{@key[procedure] Insert(@*@ @ @ C: @key[in out] T; Before: C;@*@ @ @ New_Item: E_T; Position: @key[out] Cursor;@*@ @ @ Count: C_T := 1);}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@\@\@\
@s{@shrink{@key[procedure] Insert(@*@ @ @ C: @key[in out] T; Before: C;@*@ @ @ Position: @key[out] Cursor; Count: C_T := 1);@*element has default value}}@\@s{@shrink{Y@*also Index}}@\@s{@shrink{Y}}@\@\@\@\
@s{@shrink{@key[procedure] Insert(@*@ @ @ C: @key[in out] T; Key: K_T;@*@ @ @ New_Item: E_T; Position: @key[out] Cursor;@*@ @ @ Inserted: @key[out] B);}}@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y (no key)}}@\@s{@shrink{Y (no key)}}
@s{@shrink{@key[procedure] Insert(@*@ @ @ C: @key[in out] T; Key: K_T;@*@ @ @ Position: @key[out] Cursor; Inserted: @key[out] B);@*element has default value}}@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@\
@s{@shrink{@key[procedure] Insert(@*@ @ @ C: @key[in out] T; Key: K_T;@*@ @ @ New_Item: E_T);}}@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y (no key)}}@\@s{@shrink{Y (no key)}}
@s{@shrink{@key[procedure] Prepend(@*@ @ @ C: @key[in out] Vector;@*@ @ @ New_Item: Vector);}}@\@s{@shrink{Y}}@\@\@\@\@\
@s{@shrink{@key[procedure] Prepend(@*@ @ @ C: @key[in out] T;@*@ @ @ New_Item: E_T; Count: C_T := 1);}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@\@\@\
@s{@shrink{@key[procedure] Append(@*@ @ @ C: @key[in out] Vector;@*@ @ @ New_Item: Vector);}}@\@s{@shrink{Y}}@\@\@\@\@\
@s{@shrink{@key[procedure] Append(@*@ @ @ C: @key[in out] T;@*@ @ @ New_Item: E_T; Count: C_T := 1);}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@\@\@\
@s{@shrink{@key[procedure] Insert_Space(@*@ @ @ C: @key[in out] V; Before: Cursor;@*@ @ @ Position: @key[out] Cursor; Count: C_T := 1);}}@\@s{@shrink{Y@*also Index}}@\@\@\@\@\
@s{@shrink{@key[procedure] Include(@*@ @ @ C: @key[in out] T;@*@ @ @ Key: Key_Type; New_Item: E_T);}}@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y (no key)}}@\@s{@shrink{Y (no key)}}
@s{@shrink{@key[procedure] Replace(@*@ @ @ C: @key[in out] T;@*@ @ @ Key: Key_Type; New_Item: E_T);}}@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y (no key)}}@\@s{@shrink{Y (no key)}}
@s{@shrink{@key[procedure] Exclude(@*@ @ @ C: @key[in out] T;@*@ @ @ Key: Key_Type);}}@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y (Item not key)}}@\@s{@shrink{Y (item not key)}}
@s{@shrink{@key[procedure] Delete(@*@ @ @ C: @key[in out] T; P: @key[in out] C;@*@ @ @ Count: C_T := 1);}}@\@s{@shrink{Y@*also Index}}@\@s{@shrink{Y}}@\@s{@shrink{Y (no count)@*also Key}}@\@s{@shrink{Y (no count)@*also Key}}@\@s{@shrink{Y (no count)@*also Element}}@\@s{@shrink{Y (no count)@*also Element}}
@s{@shrink{@key[procedure] Delete_First(@*@ @ @ C: @key[in out] T; Count: C_T := 1);@*@key[procedure] Delete_Last(@*@ @ @ C: @key[in out] T; Count: C_T := 1);}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@\@s{@shrink{Y (no count)}}@\@\@s{@shrink{Y (no count)}}
@s{@shrink{@key[procedure] Reverse_Elements(C: @key[in out] T);}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@\@\@\
@s{@shrink{@key[procedure] Swap(C: @key[in out] T; I, J: Cursor);}}@\@s{@shrink{Y@*also Index}}@\@s{@shrink{Y}}@\@\@\@\
@s{@shrink{@key[procedure] Swap_Links(C: @key[in out] List; I, J: Cursor);}}@\@\@s{@shrink{Y}}@\@\@\@\
@s{@shrink{@key[procedure] Splice(@*@ @ @ Target: @key[in out] List; Before: Cursor;@*@ @ @ Source: @key[in out] List);@*@key[procedure] Splice(@*@ @ @ Target: @key[in out] List; Before: Cursor;@*@ @ @ Source: @key[in out] List; Position: @key[in out] Cursor);@*@key[procedure] Splice(@*@ @ @ Container: @key[in out] List; Before: Cursor;@*@ @ @ Position: @key[in out] Cursor);}}@\@\@s{@shrink{Y}}@\@\@\@\
@s{@shrink{@key[procedure] Union(Target: @key[in out] Set; Source: Set);@*@key[function] Union(L, R: Set) @key[return] Set;@*@key[function] "or" (L, R: Set) @key[return] Set @key[renames] Union;}}@\@\@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[procedure] Intersection(@*@ @ @ Target: @key[in out] Set;@*@ @ @ Source: Set);@*@key[function] Intersection(L, R: Set) @key[return] Set;@*@key[function] "and" (L, R: Set) @key[return] Set@*@ @ @ @key[renames] Intersection;}}@\@\@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[procedure] Difference(@*@ @ @ Target: @key[in out] Set;@*@ @ @  Source: Set);@*@key[function] Difference(L, R: Set) @key[return] Set;@*@key[function] "@en" (L, R: Set) @key[return] Set@*@ @ @ @key[renames] Difference;}}@\@\@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[procedure] Symmetric_Difference(@*@ @ @ Target: @key[in out] Set;@*@ @ @ Source: Set);@*@key[function] Symmetric_Difference (L, R: Set) @key[return] Set;@*@key[function] "xor" (L, R: Set) @key[return] Set@*@ @ @ @key[renames] Symmetric_Difference;}}@\@\@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[function] Overlap(L, R: Set) @key[return] Boolean;@*@key[function] Is_Subset(Subset: Set; Of_Set: Set)@*@ @ @ @key[return] B;}}@\@\@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[function] First_Index(C: T) @key[return] Index_Type;}}@\@s{@shrink{Y}}@\@\@\@\@\
@s{@shrink{@key[function] First(C: T) @key[return] Cursor;}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[function] First_Element(C: T)@*@ @ @ @key[return] Element_Type;}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@\@s{@shrink{Y}}@\@\@s{@shrink{Y}}
@s{@shrink{@key[function] First_Key(C: T) @key[return] Key_Type;}}@\@\@\@\@s{@shrink{Y}}@\@\
@s{@shrink{@key[function] Last_Index(C: T) @key[return] Ex_Ind;}}@\@s{@shrink{Y}}@\@\@\@\@\
@s{@shrink{@key[function] Last(C: T) @key[return] Cursor;}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@\@s{@shrink{Y}}@\@\@s{@shrink{Y}}
@s{@shrink{@key[function] Last_Element(C: T)@*@ @ @ @key[return] Element_Type;}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@\@s{@shrink{Y}}@\@\@s{@shrink{Y}}
@s{@shrink{@key[function] Last_Key(C: T) @key[return] Key_Type;}}@\@\@\@\@s{@shrink{Y}}@\@\
@s{@shrink{@key[function] Next(P: C) @key[return] Cursor;@*@key[procedure] Next(P: @key[in out] C);}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[function] Previous(P: C) @key[return] Cursor;@*@key[procedure] Previous(P: @key[in out] C);}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@\@s{@shrink{Y}}@\@\@s{@shrink{Y}}
@s{@shrink{@key[function] Find_Index(@*@ @ @ C: T; Item: E_T;@*@ @ @ Index: I_T := I_T'First) @key[return] Ex_Ind;}}@\@s{@shrink{Y}}@\@\@\@\@\
@s{@shrink{@key[function] Find(C: T; ... ; P: C := No_Element)@*@ @ @ @key[return] Cursor;}}@\@s{@shrink{Element}}@\@s{@shrink{Element}}@\@s{@shrink{Key (no position)}}@\@s{@shrink{Key (no position)}}@\@s{@shrink{Element (no position)}}@\@s{@shrink{Element (no position)}}
@s{@shrink{@key[function] Element(C: T; Key: K_T) @key[return] E_T;}}@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@\
@s{@shrink{@key[function] Reverse_Find_Index(@*@ @ @ C: T; Item: E_T;@*@ @ @ Index: I_T := I_T'First) @key[return] Ex_Ind;}}@\@s{@shrink{Y}}@\@\@\@\@\
@s{@shrink{@key[function] Reverse_Find(C: T; ... ; P: C := No_Element)@*@ @ @ @key[return] Cursor;}}@\@s{@shrink{Element}}@\@s{@shrink{Element}}@\@\@\@\
@s{@shrink{@key[function] Floor(C: T; ...) @key[return] Cursor;@*@key[function] Ceiling(C: T; ...) @key[return] Cursor;}}@\@\@\@\@s{@shrink{Key: K_T}}@\@\@s{@shrink{Item: E_T}}
@s{@shrink{@key[function] Contains(C: T; ...) @key[return] Boolean;}}@\@s{@shrink{Element}}@\@s{@shrink{Element}}@\@s{@shrink{Key}}@\@s{@shrink{Key}}@\@s{@shrink{Element}}@\@s{@shrink{Element}}
@s{@shrink{@key[function] Has_Element(P: C) @key[return] Boolean;}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[function] Equivalent_... (L, R: Cursor)@*@ @ @ @key[return] Boolean;@*@key[function] Equivalent_... (L: Cursor; R:...)@*@ @ @ @key[return] Boolean;@*@key[function] Equivalent_... (L:...; R: Cursor)@*@ @ @ @key[return] Boolean;}}@\@\@\@s{@shrink{Keys}}@\@\@s{@shrink{Elements}}@\
@s{@shrink{@key[function] "<" (L, R: Cursor) @key[return] Boolean;@*@key[function] ">" (L, R: Cursor) @key[return] Boolean;@*@key[function] "<" (L, Cursor; R: ...) @key[return] Boolean;@*@key[function] ">" (L, Cursor; R: ...) @key[return] Boolean;@*@key[function] "<" (L:...; R: Cursor) @key[return] Boolean;@*@key[function] ">" (L:...; R: Cursor) @key[return] Boolean;}}@\@\@\@\@S{@shrink{Key}}@\@\@s{@shrink{Element}}
@s{@shrink{@key[procedure] Iterate(C: @key[in] T;@*@ @ @ Process: @key[not null acc proc] (P: C) );}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[procedure] Reverse_Iterate(C: in T;@*@ @ @ Process: @key[not null acc proc] (P: C) );}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@\@s{@shrink{Y}}@\@\@s{@shrink{Y}}
@s{@shrink{@b[generic]@*@ @ @ @key[with] @key[function] "<" (Left, Right: E_T) @key[return] B is <>;@*@key[package] Generic_Sorting @key[is]@*@ @ @ @key[function] Is_Sorted(C: T) @key[return] Boolean;@*@ @ @ @key[procedure] Sort(C: @key[in out] T);@*@ @ @ @key[procedure] Merge(Target, Source: @key[in out] T);@*@key[end] Generic_Sorting;}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@\@\@\
@s{@shrink{@b[generic]@*@ @ @ @key[type] Key_Type (<>) @key[is private];}}@\@\@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@ @ @key[with function] Key(Element: E_T) @key[return] Key_Type;}}@\@\@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@ @ @key[with function] Hash(Key: K_T) @key[return] Hash_Type;}}@\@\@\@\@\@s{@shrink{Y}}@\
@s{@shrink{@ @ @key[with function] Equivalent_Keys (L, R: Key_Type)@*@ @ @ @ @ @ @key[return] Boolean;}}@\@\@\@\@\@s{@shrink{Y}}@\
@s{@shrink{@ @ @key[with function] "<" (L, R: Key_Type) @key[return] B is <>;}}@\@\@\@\@\@\@s{@shrink{Y}}
@s{@shrink{@key[package] Generic_Keys @key[is]}}@\@\@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[function] Equivalent_Keys(L, R: Key_Type) @key[return] B;}}@\@\@\@\@\@\@s{@shrink{Y}}
@s{@shrink{@key[function] Key(P: C) @key[return] Key_Type;}}@\@\@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[function] Element(C: T; Key: K_T) @key[return] Element_T;}}@\@\@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[procedure] Replace(C: in out T; Key: Key_Type;@*@ @ @ New_Item: E_T);@*@key[procedure] Exclude(C: @key[in out] T; Key: Key_Type);@*@key[procedure] Delete(C: @key[in out] T; Key: Key_Type);}}@\@\@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[function] Find(C: T; Key: K_T) @key[return] Cursor;}}@\@\@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[function] Floor(C: T; Key: K_T) @key[return] Cursor;@*@key[function] Ceiling(C: T; Key: K_T) @key[return] Cursor;}}@\ @\ @\ @\ @\ @\@s{@shrink{Y}}
@s{@shrink{@key[function] Contains(C: T; Key: K_T) @key[return] Boolean;}}@\ @\ @\ @\ @\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[procedure] Update_Element_Preserving_Key@*@ @ @ (C: in out T; P: C;@*@ @ @ Process: @key[not null acc proc] (Element: @key[in out] E_T) );}}@\ @\ @\ @\ @\@s{@shrink{Y}}@\@s{@shrink{Y}}
@s{@shrink{@key[end] Generic_Keys;}}@\@\@\@\@\@s{@shrink{Y}}@\@s{@shrink{Y}}@last
@s{@shrink{@key[private]@*  ... -- @i[not specified by the language]@*@key[end] Ada.Containers....;}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}@\@s{@shrink{Y}}]}
