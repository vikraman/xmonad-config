
Config  { font            = "-*-fixed-bold-r-normal-*-12-*-*-*-*-*-*-*"
        , bgColor         = "black"
        , fgColor         = "#BBFFFF"
        , position        = Bottom
        , lowerOnStart    = False
        , commands        = [ Run StdinReader
                            , Run MPD ["-t","<artist> - <title> <statei> ","--", "-P", ">>", "-Z", "|", "-S", "><"] 10
                            , Run MultiCpu ["-t","<total0>% <total1>% <total2>% <total3>%","-L","10","-H","50","-l","green","-n","orange","-h","red"] 40
                            , Run CoreTemp ["-t", "<core0>C <core1>C","-L","60","-H","75","-l","green","-n","orange","-h","red"] 50
                            , Run Memory ["-t","<used>M <cache>M","-L","256","-H","1024","-l","green","-n","orange","-h","red"] 60
                            , Run Network "wlan0" ["-t","<rx>K <tx>K","-L","10","-H","100","-l","green","-n","orange","-h","red"] 20
                            , Run BatteryP ["BAT0"] ["-t","<left>%","-L","25","-H","90","-l","red","-n","orange","-h","green"] 10
                            , Run Weather "VILK" ["-t","<tempC>C","-L","20","-H","30","-l","green","-n","orange","-h","red"] 36000
                            , Run Date "<fc=#00CCFF>%a</fc> <fc=#33FFCC>%b %d</fc> <fc=#FFDD00>%H:%M:%S</fc>" "date" 10
                            ]
        , sepChar         = "%"
        , alignSep        = "}{"
        , template        = "<fc=#FF3333>%StdinReader%</fc> }{ %mpd% | %multicpu% | %coretemp% | %memory% | %wlan0% | %battery% | %VILK% | %date%"
        }
