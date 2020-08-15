# ada-auth ARM

This is a Git mirror of
[Ada Conformity Assessment Authority](http://www.ada-auth.org/)
[CVS repository](http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/).
It contains

 * Ada Reference Manual
 * ACATS User Guide
 * Source code of Ada formatting tool

See details about Ada formatting tool in [command.txt](progs/command.txt)

To build it with GNAT I provide a project file (ada_form.gpr),
that isn't a part of CVS repository. Just run:

```
gprbuild -p -P ada_form.gpr
```

If you use case sensitive filesystem you need edit master file converting
file names to lowercase:

```
cd source
sed -i -e '/@Source/s/<[^>]*>/\L\0/' aa-aarm.msm
mkdir output
../.obj/ada_form aa-aarm.msm
```

## Generated HTML documents

| Version        | Annotated | No Changes  | Prev Changes | All Changes |
| -------------- | --------- | ----------- | ------------ | ----------- |
| Ada 202x Draft | Standard  | [rm-5-NC]   | [rm-5-PC]    | [rm-5-AC]   |
| Ada 202x Draft | Annotated | [aarm-5-NC] | [aarm-5-PC]  | [aarm-5-AC] |
| Ada 2012 TC1   | Standard  | [rm-4-NC]   | [rm-4-PC]    | [rm-4-AC]   |
| Ada 2012 TC1   | Annotated | [aarm-4-NC] | [aarm-4-PC]  | [aarm-4-AC] |
| Ada 2012       | Standard  | [rm-3-NC]   | [rm-3-PC]    | [rm-3-AC]   |
| Ada 2012       | Annotated | [aarm-3-NC] | [aarm-3-PC]  | [aarm-3-AC] |
| Ada 2005       | Standard  | [rm-2-NC]   | [rm-2-PC]    | [rm-2-AC]   |
| Ada 2005       | Annotated | [aarm-2-NC] | [aarm-2-PC]  | [aarm-2-AC] |
| Ada 95         | Standard  | [rm-1-NC]   | [rm-1-PC]    | [rm-1-AC]   |
| Ada 95         | Annotated | [aarm-1-NC] | [aarm-1-PC]  | [aarm-1-AC] |


[rm-5-NC]:   https://reznikmm.github.io/ada-auth/rm-5-NC/RM-TTL.html
[aarm-5-NC]: https://reznikmm.github.io/ada-auth/aarm-5-NC/AA-TTL.html
[rm-5-PC]:   https://reznikmm.github.io/ada-auth/rm-5-PC/RM-TTL.html
[aarm-5-PC]: https://reznikmm.github.io/ada-auth/aarm-5-PC/AA-TTL.html
[rm-5-AC]:   https://reznikmm.github.io/ada-auth/rm-5-AC/RM-TTL.html
[aarm-5-AC]: https://reznikmm.github.io/ada-auth/aarm-5-AC/AA-TTL.html
[rm-4-NC]:   https://reznikmm.github.io/ada-auth/rm-4-NC/RM-TTL.html
[aarm-4-NC]: https://reznikmm.github.io/ada-auth/aarm-4-NC/AA-TTL.html
[rm-4-PC]:   https://reznikmm.github.io/ada-auth/rm-4-PC/RM-TTL.html
[aarm-4-PC]: https://reznikmm.github.io/ada-auth/aarm-4-PC/AA-TTL.html
[rm-4-AC]:   https://reznikmm.github.io/ada-auth/rm-4-AC/RM-TTL.html
[aarm-4-AC]: https://reznikmm.github.io/ada-auth/aarm-4-AC/AA-TTL.html
[rm-3-NC]:   https://reznikmm.github.io/ada-auth/rm-3-NC/RM-TTL.html
[aarm-3-NC]: https://reznikmm.github.io/ada-auth/aarm-3-NC/AA-TTL.html
[rm-3-PC]:   https://reznikmm.github.io/ada-auth/rm-3-PC/RM-TTL.html
[aarm-3-PC]: https://reznikmm.github.io/ada-auth/aarm-3-PC/AA-TTL.html
[rm-3-AC]:   https://reznikmm.github.io/ada-auth/rm-3-AC/RM-TTL.html
[aarm-3-AC]: https://reznikmm.github.io/ada-auth/aarm-3-AC/AA-TTL.html
[rm-2-NC]:   https://reznikmm.github.io/ada-auth/rm-2-NC/RM-TTL.html
[aarm-2-NC]: https://reznikmm.github.io/ada-auth/aarm-2-NC/AA-TTL.html
[rm-2-PC]:   https://reznikmm.github.io/ada-auth/rm-2-PC/RM-TTL.html
[aarm-2-PC]: https://reznikmm.github.io/ada-auth/aarm-2-PC/AA-TTL.html
[rm-2-AC]:   https://reznikmm.github.io/ada-auth/rm-2-AC/RM-TTL.html
[aarm-2-AC]: https://reznikmm.github.io/ada-auth/aarm-2-AC/AA-TTL.html
[rm-1-NC]:   https://reznikmm.github.io/ada-auth/rm-1-NC/RM-TTL.html
[aarm-1-NC]: https://reznikmm.github.io/ada-auth/aarm-1-NC/AA-TTL.html
[rm-1-PC]:   https://reznikmm.github.io/ada-auth/rm-1-PC/RM-TTL.html
[aarm-1-PC]: https://reznikmm.github.io/ada-auth/aarm-1-PC/AA-TTL.html
[rm-1-AC]:   https://reznikmm.github.io/ada-auth/rm-1-AC/RM-TTL.html
[aarm-1-AC]: https://reznikmm.github.io/ada-auth/aarm-1-AC/AA-TTL.html


