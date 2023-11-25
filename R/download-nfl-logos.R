url <- "https://en.wikipedia.org/wiki/Seattle_Seahawks#/media/File:Seattle_Seahawks_logo.svg"
download.file(url, destfile = file.path("input", "seahawks.svg"), mode = "wb")

urls <- c(
"https://upload.wikimedia.org/wikipedia/en/thumb/7/77/Buffalo_Bills_logo.svg/200px-Buffalo_Bills_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/8/8e/Seattle_Seahawks_logo.svg/200px-Seattle_Seahawks_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/commons/thumb/1/15/Dallas_Cowboys.svg/200px-Dallas_Cowboys.svg.png",
"https://upload.wikimedia.org/wikipedia/commons/thumb/3/3a/San_Francisco_49ers_logo.svg/200px-San_Francisco_49ers_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/commons/thumb/6/60/New_York_Giants_logo.svg/200px-New_York_Giants_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/6/6b/New_York_Jets_logo.svg/200px-New_York_Jets_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/3/37/Miami_Dolphins_logo.svg/200px-Miami_Dolphins_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/b/b9/New_England_Patriots_logo.svg/200px-New_England_Patriots_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/commons/thumb/5/50/Green_Bay_Packers_logo.svg/200px-Green_Bay_Packers_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/1/16/Baltimore_Ravens_logo.svg/200px-Baltimore_Ravens_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/commons/thumb/8/81/Cincinnati_Bengals_logo.svg/200px-Cincinnati_Bengals_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/d/d9/Cleveland_Browns_logo.svg/200px-Cleveland_Browns_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/commons/thumb/d/de/Pittsburgh_Steelers_logo.svg/200px-Pittsburgh_Steelers_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/2/28/Houston_Texans_logo.svg/200px-Houston_Texans_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/commons/thumb/0/00/Indianapolis_Colts_logo.svg/200px-Indianapolis_Colts_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/7/74/Jacksonville_Jaguars_logo.svg/200px-Jacksonville_Jaguars_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/c/c1/Tennessee_Titans_logo.svg/200px-Tennessee_Titans_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/4/44/Denver_Broncos_logo.svg/200px-Denver_Broncos_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/e/e1/Kansas_City_Chiefs_logo.svg/200px-Kansas_City_Chiefs_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/8/8e/Philadelphia_Eagles_logo.svg/200px-Philadelphia_Eagles_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/commons/thumb/0/0c/Washington_Commanders_logo.svg/200px-Washington_Commanders_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/1/15/Chicago_Bears_logo_primary.svg/200px-Chicago_Bears_logo_primary.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/7/71/Detroit_Lions_logo.svg/200px-Detroit_Lions_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/4/48/Minnesota_Vikings_logo.svg/195px-Minnesota_Vikings_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/c/c5/Atlanta_Falcons_logo.svg/200px-Atlanta_Falcons_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/1/1c/Carolina_Panthers_logo.svg/200px-Carolina_Panthers_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/commons/thumb/5/50/New_Orleans_Saints_logo.svg/197px-New_Orleans_Saints_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/Tampa_Bay_Buccaneers_logo.svg/200px-Tampa_Bay_Buccaneers_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/7/72/Arizona_Cardinals_logo.svg/200px-Arizona_Cardinals_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/8/8a/Los_Angeles_Rams_logo.svg/200px-Los_Angeles_Rams_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/en/thumb/4/48/Las_Vegas_Raiders_logo.svg/200px-Las_Vegas_Raiders_logo.svg.png",
"https://upload.wikimedia.org/wikipedia/commons/thumb/a/a6/Los_Angeles_Chargers_logo.svg/200px-Los_Angeles_Chargers_logo.svg.png"
)

filenames <- str_extract(urls, "px-(.+?.png)", group = 1) %>% str_remove("_logo|_primary")

walk2(urls, filenames,
      ~download.file(.x, destfile = file.path("input", "nfl-logos", .y), mode = "wb"))
