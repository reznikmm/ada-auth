Move .\Object\%1.Obj .
link  -subsystem:console -entry:mainCRTStartup -out:%1.exe %1.obj libc.lib kernel32.lib -map:%1.map
Del %1.Obj
Del %1.Map
Move %1.Exe ..\Source
