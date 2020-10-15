    ##  (´-`) .｡oO (Common function, 2020-01-21)

    ##  General parameters  == (2020-02-05) ========================
    gp.  <-  function (...) {
        #  skipMess. (suppressPackageStartupMessages ('easypackages' :: libraries (c ('bindrcpp', 'lubridate', 'naturalsort', 'readxl', 'tidyverse'))))
        #  if (dev.list () > 0)  dev.new (width = 3 * (1+ sqrt (5))/2, height = 3)    #  4.5, 3.3
        #  quartz.options (width = 5.682819, height = 3.004405); dev.new ();  par (mar = c (2.4, 3.3, 1.1, 2.8), tcl = 0.35)
        #  windows (width = 4.3, height = 3.3)    #  for Windows
            par (mgp = c (0, 0.2, 0), ann = F, xaxs = 'i', yaxs = 'i', col = 'grey13', col.axis = 'grey13', fg = 'grey13', ps = 13, lwd = 1.3, cex.axis = 1, las = 1)
            par (mar = c (2.4, 3.3, 0.5, 1), tcl = 0.25, family = ifelse (Sys.getenv ('OS') == '', 'Avenir Next', 'sans'))    # Ubuntu  Avenir  Open Sans  Light
            formals (axis) [c ('col.ticks', 'lwd.ticks', 'lwd')] <-  list ('grey13', 1.3, 0)
    }

    ##  Skip warning messages ================================================
    skipMess.  <-  function (x)  suppressWarnings (suppressMessages (invisible (x)))

    ##  Path control  == (2019-11-04) ================================================
    setwd.  <-  function (...) {
            chr  <-  pp. ()
            if (Sys.getenv ('OS') == '') {    #  For Mac;  You must copy a file path shown on the editor after drag & drop of the concerned file
                    chr  %>%  {if (str_detect (., pattern = 'csv$|xls$|xlsx$'))  dirname (.)  else  .}  %>%  setwd (.)
            } else {    #  For Windows;  You must copy a file path shown on file folder
                    if (! str_detect (chr, pattern = '\\\\'))  stop ('Not available file path...\n\n', call. = F)
                    gsub ('\\\\', '\\/', chr)  %>%  setwd (.)
            }
    }    #  setwd. (pp. ())

    ##  Lightly vroom ()  == (2020-02-06) ========================
    vroom.  <-  function (filename, col_names = T, skip = 0, n_max = Inf) {
            if (Sys.getenv ('OS') != '' & str_detect (filename, pattern = '\\p{Hiragana}|\\p{Katakana}|\\p{Han}')) {
                    out  <-  skipMess. (read_csv (filename, locale = locale (encoding  = 'cp932'), col_names = col_names, skip = skip, n_max = n_max))
            } else {
                    out  <-  skipMess. ('vroom' :: vroom (filename, locale = locale (encoding  = 'cp932'), col_names = col_names, skip = skip, n_max = n_max))
            }
            return (out)
    }

    ##  Reading data  == (2020-03-31) ================================================
    getData.  <-  function (filePath = NULL, file = NULL, timeSort = F, timeFactor = NULL, ... ) {    #  Choose time vector if you have many time vectors in data
            ##  Note:  Quick reading with no title to search for the border between colnames and data
            oldDir  <-  getwd ()
            if (! is.null (filePath))  setwd (filePath)
            if (is.null (file)) {
                    file  <-  dir (pattern = 'csv|xls|xlsx') %>%  {. [! str_detect (., '\\$')]}  %>%  {if (length (.) > 1) {chooseOne. (., '\"Target File\"')} else {.}}
                    if (length (file) == 0)  stop ('No data file in this directory...\n\n', call. = F)
            } else if (length(file) > 1) {
                    file <- chooseOne. (file, '\"Target File\"')
            }
            readDt  <-  function (file, col_names = F, skip = 0, n_max = Inf, ... ) {
                    if (str_detect (file, pattern = 'csv')) {
                          #  dt  <-  skipMess. (read_csv (file, locale = locale (encoding  = 'cp932'), col_names = col_names, skip = skip, n_max = n_max))
                            dt  <-  vroom. (file, col_names = col_names, skip = skip, n_max = n_max)
                    } else if (str_detect (file, pattern = 'xls|xlsx')) {    #  Many excel files will be OK, only if each of them has common column and one sheet.
                            dt  <-  skipMess. (read_excel (file, sheet = 1, col_names = col_names, skip = skip, n_max = n_max))
                    }
                    dt  %>%  'dplyr' :: filter (rowSums (is.na (.)) != ncol (.))#  %>%  return (.)
            }
            ##  Title inference:  twice reading
            getDt  <-  function (...) {
                    dt_name0  <-  readDt (file, n_max = 20) %>%  'dplyr' :: filter (rowSums (is.na (.)) != ncol (.))    # 1st reading to minimize the processing burden
                    types_start  <-  map. (dt_name0, ~ type_sum (.))    #  all <chr> type or ...
                    for (i in seq (nrow (dt_name0))) {
                            types_i  <-  dt_name0 [(i: nrow (dt_name0)), ]  %>%  {skipMess. (type_convert (.))}  %>%  map. (., ~ type_sum (.))
                            if (! setequal (types_start, types_i)) {
                                    skip_num  <-  1: (i -1)
                                    break
                            } else if (i == nrow (dt_name0) && setequal (types_start, types_i)) {
                                    skip_num  <-  0
                            }
                    }
                    ##  2nd reading;  full reading
                    if (skip_num == 0) {
                            dt  <-  readDt (file, col_names = F)
                    } else if (skip_num == 1) {
                            dt  <-  readDt (file, col_names = T)
                    }
                    if (skip_num > 1) {
                            title_names  <-  dt_name0 [(1: skip_num), ]  %>%   map_df (., ~ str_flatten (., collapse = ' :: '))  %>%
                                                    unlist (.) %>%  zenk. (.)  %>%  str_trim (., side = 'both')
                            dt  <-  readDt (file, col_names = F, skip = skip_num)  %>%  setNames (., title_names)
                    }
                    return (dt)
            }
            dt  <-  getDt ()
            File  <<-  file    #  Someday useful
            if (! is.null (filePath))  setwd (oldDir)
            dt2time. (dt, timeSort, timeFactor)  %>%  return (.)
    }    #  END of getData. ()


    ##  Find whether quasi-time format or not  == (2020-02-01) ================================================
    lazyAny.  <-  function (x, Fun = is.na, ... ) {
            for (i in seq_along (x))  if (! is.na (x [i]) && Fun (x [i]))  {return (TRUE); break}  else if (! is.na (x [i]) && ! Fun (x [i]))  {return (FALSE); break}
    }
    is_time.  <-  function (x, ... )  is.POSIXct (x) | is.Date (x)
    is_quasi_time.  <-  function (x, ... ) {
            if (all (is.na (x)))  return (F)
            posTime_TF  <-  is.POSIXct (x)
            dateTime_TF  <-  is.Date (x)
            is_digit  <-  function (x)  x  %>%  {str_detect (., '[:digit:]') && ! str_detect (., '[:upper:]|[:lower:]')}  %>%  {if (! is.na (.))  .  else  F} #  No alphabet
            digit_TF  <-  lazyAny. (x, Fun = is_digit)
            is_timeChr  <-  function (x)  x  %>%  {str_count (., '/') == 2 || str_count (., '-') == 2 && ! str_count (., '/|-') > 2}  %>%  {if (! is.na (.))  .  else  F}
            chr_TF  <-  lazyAny. (x, Fun = is_timeChr)
            return (! posTime_TF & ! dateTime_TF & digit_TF & chr_TF)
    }    #  c ( '2019/1/8', '2019/1/8 12:34', '2019/1/8 12:34:56', '2019-11-14', '2019-11-14 12:34', '2019-11-14 12:34:56' )


    ##  Time style conversion in the tibble level  == (2020-02-02) ================================================
    dt2time.  <-  function (dt, timeSort = F, timeFactor = NULL, ... ) {    #  Use this by getData. () & pp. ()
            ##  Time style conversion in the vector level
            chr2time.  <-  function (chrVec, ... ) {
                    if (is.POSIXct (chrVec))  return (chrVec)
                    if (is.Date (chrVec))  return (as_datetime (chrVec))
                    if (is_quasi_time. (chrVec) == T) {
                            timeVec  <-  as.POSIXct (rep (NA, length (chrVec)), tz = 'Asia/Tokyo')    #  Make an empty vector of POSIXct
                            for (i in seq_along (chrVec)) {
                                    tenta  <-  whichSize. (ref = str_count (chrVec [i], ':'), vec = 0:2, c ('Ymd', 'YmdHM', 'YmdHMS'))
                                    timeVec [i]  <-  c (chrVec [i], tenta)  %>%  {if (anyNA (.))  NA  else  parse_date_time2 (. [1], orders = . [2], tz = 'Asia/Tokyo') }
                            }    #  The function parse_date_time2 () only permits not vector but element, as well as gsub ()...
                            return (timeVec)
                    } else {
                            return (chrVec)    #  Return chrVec as raw
                    }
            }
            ##  Then, time converting to the tibble;  if dt has no time possibility,  it will return dt with no change
            if (is.atomic (dt))  return (chr2time. (dt))
            if (is.data.frame (dt) && nrow (dt) == 0) return (dt)    #  Safety net for pp. () when copying a mere cell as vector
            dt  <-  dt  %>%  mutate_if (~ is_quasi_time. (.), ~ chr2time. (.) )

            ##  New colulmn and sorting with time series
            timeColN  <-  map_lgl (dt, ~ is_time. (.))  %>% names (dt) [.]
            if (timeSort == F) {    #  If timeSort is false, it doesn't need to create 'Time' new column.
                    return (dt)
            } else if (timeSort == T && length (timeColN) != 0) {    #  Add just one 'Time' column into dt to distinguish the favorite from time columns.
                    if (! is.null (timeFactor)) {
                            dt  <-  dt [[timeFactor]]  %>% {if (is.null (.))  NA  else  .}  %>%  mutate (dt, Time = .)    # To avoid wrong spelling
                    } else {
                            if (length (timeColN) == 1)  dt  <- select_if (dt, ~ is_time. (.))  %>%  pull (.)  %>%  mutate (dt, Time = .)
                            if (length (timeColN)  > 1)  dt <-  chooseOne. (timeColN, '\"TIME factor\"')  %>%  dt [[.]] %>%  mutate (dt, Time = .)
                    }
                    if (dt $ 'Time' %>% {length ( . [is.na (.)]) / length (.) > 0.15})  dt  <-  'dplyr' :: filter (dt, ! is.na (Time))    # tv with too many NA is crap (15 %)
                    dt  <-  arrange (dt, Time)    #  Ascending sort in time vector
                    return (dt)
            }
    }

    ##  Powerful copy & paste  == (2020-01-23) ================================================
    pp.  <-  function (...) {
            name01  <-  function (clip2, ... ) {
                    dt_name0  <-  clip2 [1:10, ]  %>%  'dplyr' :: filter (rowSums (is.na (.)) != ncol (.))    #  1st reading to minimize the burden of processing of reading
                    types_start  <-  map. (dt_name0, ~ type_sum (.)) %>% str_replace (., 'lgl', 'chr')    #  all <chr> type;  <lgl> is made of blank cells
                    for (i in seq (nrow (dt_name0))) {
                            types_i  <-  dt_name0 [(i: nrow (dt_name0)), ]  %>%  {skipMess. (type_convert (.))}  %>% dt2time. (., timeSort = F)  %>%
                                             map. (., ~ . [! is.na (.)] %>% type_sum (.) %>% str_replace (., 'lgl', 'chr'))    # Avoid all NA column reconverted to <lgl>
                            if (! setequal (types_start, types_i)) {
                                    skip_num  <-  i -1
                                    break
                            } else if (i == nrow (dt_name0) && setequal (types_start, types_i)) {
                                    skip_num  <-  0
                            }
                    }
                    ##  2nd reading;  full reading
                    if (skip_num == 0) {
                            clip2  <-  clip2  %>%  setNames (., str_c ('#', seq_along (names (clip2))))
                    } else if (skip_num == 1) {
                            title_names  <-  clip2 [1, ]  %>% unlist (.)  %>%  zenk. (.)  %>%  str_trim (., side = 'both')
                            which_blank  <-  which (is.na (title_names))
                            clip2  <-  if (length (which_blank) == 0) {
                                    clip2 [-1, ]  %>%  setNames (., title_names)  %>%  {skipMess. (type_convert (.))}
                            } else {    #  Kill the blank column only after realizing skip_num = 1
                                    clip2 [-1, -which_blank] %>%  setNames (., title_names [-which_blank])  %>%  {skipMess. (type_convert (.))}
                            }
                    }
                    if (skip_num > 1) {
                            title_names  <-  dt_name0 [(1: skip_num), ]  %>%   map_df (., ~ str_flatten (., collapse = ' :: '))  %>%
                                                    unlist (.) %>%  zenk. (.)  %>%  str_trim (., side = 'both')
                            clip2  <-  clip2 [-1: -skip_num, ] %>%  setNames (., title_names)  %>%  {skipMess. (type_convert (.))}
                    }
                    return (clip2)
            }
            correctChr  <-  function (chr, ... ) {
                    if (str_detect (chr, '[:alpha:]') %>% any. (.) %>% `!`) {
                            if (str_detect (chr, '%') %>% any. (.))  chr  <-  as.vector (chr)  %>%  parse_number (.)  %>% {. /100}    #  "12.3%"
                            if (str_detect (chr, ',') %>% any. (.))  chr  <-  as.vector (chr)  %>%  gsub (',', '', .) %>%  parse_number (.)    #  "123,456,789", or "\1,000"
                    }
                    chr [chr %in% '']  <-  NA    #  str_detect (chr, '') doesn't work well...
                    chr  <-  zenk. (chr)
                    return (chr)
            }
            clip  <-  clipboard ()
            clip2  <-  if (length (clip) > 1)  skipMess. (read_delim (clip, col_names = F, delim = '\t'))  else  str_split (clip, '\t') [[1]]
            clip3  <-  if (is.list (clip2)) {
                    clip2  %>%  'dplyr' :: filter (rowSums (is.na (.)) != ncol (.))  %>%  name01 (.)  %>%  dt2time. (., timeSort = F)  %>%
                    mutate_if (~ is.character (.), ~ correctChr (.))  %>%  select_if (colSums (is.na (.)) != nrow (.) | ! str_detect (names (.), 'X') )
            } else {    #  Just copying 1 row;  return a vector
                    skipMess. (as.numeric (clip2))  %>%  {if (is.na (.) %>% all (.))  clip2  else  .}
            }
            return (clip3)
    }

    ##  Transform list data to tibble  == (2019-11-26) ================================================
    list2tibble.  <-  function (dL, ... ) {
            if (is.data.frame (dL))  return (as_tibble (dL))    # Safety net;  dL is already tibble
            if (map. (dL, ~ class (.)) %>% {! 'data.frame' %in% .}) {    #  Each nested data is atomic
                    dL  <-  map (dL, ~ enframe (.) %>% . [, -1] %>% setNames ('') )    #  In case that the list is merely consisted of vectors
            } else {
                    dL  <-  map (dL, ~ as_tibble (.))    # In case that the list has a data.frame (x = ... , y = ...)
            }
            if (is.null (names (dL)))  names (dL)  <-  str_c ('List', seq_along (dL))    #  Note:  dL is all consited of tibble for now
            dtNA  <-  matrix (NA, nrow = sapply (dL, nrow) %>% max. (.), ncol = sapply (dL, ncol) %>% sum. (.))  %>% as_tibble (., .name_repair = 'minimal')
            dtName  <-  list_along (dL)
            for (i in seq_along (dL)) {
                    naCol  <-  map_df (dtNA, ~ is.na (.) %>% all (.))  %>%  { which (. == T) [1]}
                    naCols  <-  map. (dL, ~ ncol (.))  %>% cumsum (.)  %>%  {if (i == 1) 1 : . [i] else  (. [i -1] +1) : . [i]}
                    dtNA [seq (nrow (dL  [[i]])), naCols]  <- dL  [[i]]
                    dtName [[i]]  <-  if (names (dL [[i]]) == '')  names (dL) [i]  else  paste (names (dL) [i], names (dL [[i]]), sep = '.')
            }
            dL2dt  <-  dtNA  %>%  {setNames (., unlist (dtName))}
            return (dL2dt)
    }    #  list2tibble. (as.list (iris))

    ##  Transform any data to list  == (2019-12-15) ================================================
    dLformer.  <-  function (d, naturalOrder = F, ... ) {    # naturalOrder = T/F, or desirable order like c (3, 4, 1, 2) for an order of the list's names
            if ('list' %in% class (d)) {    #  Note:  if you want to draw sets of xy data, make a list of xy in advance.  Otherwise xy data will be demolished...
                    dL  <-  d
            } else if (is.atomic (d) || n_factor. (d) == 1) {    # In case of vector type or 1 column tibble
                    dL  <-  {if (is.atomic (d))  d  else  d [[1]]}  %>%  list (.)    #  No unlist () because dttm will convert  mere numeric
            } else if ('data.frame' %in% class (d)) {    #  In case of tibble or data.frame type
                    originalN  <-  names (d)
                    ##  Search character columns
                    chrTF  <-  map_lgl (d, ~ is.factor (.) | is.character (.) | is_time. (.))
                    if (any. (chrTF)) {
                            if (sum (chrTF) == 1) {
                                    names (d) [chrTF]  <-  'ID'
                            } else {    #  Select what's ID column; for starters, check unique number for each column
                                    tenta  <-  select_if (d, ~ is.factor (.) | is.character (.))  %>%  map_df (., n_distinct) %>%  {names (.) [. > 1 & . < nrow (d) ]}
                                    if (length (tenta) == 0) {    # Delete chr columns and convert it as ALL [y1, y2, ...]
                                            d  <-  select_if (d, ~ is.numeric (.))
                                            chrTF  <-  F    # overwrite
                                    }
                                    if (length (tenta) == 1)  names (d) [names (d) %in% tenta]  <-  'ID'    #  Safety net
                                    if (length (tenta) > 1 && map_lgl (d, ~ is.numeric (.)) %>% any (.)) {
                                            idName  <- chooseOne. (tenta, '\"Level Name\"')
                                            names (d) [names (d) %in% idName]  <-  'ID'
                                    }
                            }
                            ##  Time transformation for boxplot
                            if (map_lgl (d, ~ is_time. (.)) %>% any (.)) {
                                   abbre_time  <-  function (x) {    #  ex.)  humidity, temperature, yield, monthly report, ...
                                           abbreTime  <-  if (delta. (x, 'year') >= 3)  format (x, '%Y年')  else if (delta. (x, 'month') >= 4)  format (x, '%B')  else  format (x, '%H時')
                                           return (abbreTime)
                                   }
                                    d  <-  mutate_if (d, ~ is_time. (.), ~ abbre_time (.))
                                    d  <-  naturalorder (d $ 'ID')  %>%  d [., ]    #  arrange (d, ID)  does not work well
                            }
                    }
                    ##  Search numeric columns
                    numTF  <-  map_lgl (d, ~ is.numeric (.))
                    if (! any. (chrTF) && sum (numTF) >= 1) {    #  ALL [y1, y2, ...]
                            dL  <-  as.list (d)  %>%  map (., ~ . [! is.na (.)])
                    } else if (any. (chrTF) && sum (numTF) == 1) {    #  [ID, y]
                            names (d) [map_lgl (d, is.numeric)] <-  'y'
                            dL  <-  d  %>%  filter (! is.na (y))  %>%  unstack (form = y ~ ID, x = .)  %>%  as.list (.)
                            if ('res' %in% names (dL) && length (dL) == 1)  dL  <-  setNames (dL, originalN [numTF])
                    } else if (any. (chrTF) && sum (numTF) > 1) {    #  [ID, y1, y2, ...]  like iris
                            dL  <-  list ()    #  list in list structure
                            numColN  <-  which (numTF)
                            for (i in seq_along (numColN)) {
                                    tenta  <-  select (d, 'ID', numColN [i])  %>%  setNames (., c ('ID', 'y'))
                                    dL [[i]]  <-  tenta  %>% filter (! is.na (y))  %>%  unstack (form = y ~ ID, x = .) %>%  as.list (.)
                                    names (dL) [i]  <-  numColN [i]  %>%  names (d) [.]
                            }    #  You can recognized data structure to see dL [[1]] easily, but not do to see dL...
                    } else {    #  No numeric like [ID1, ID2, ...]
                            dL  <-  as.list (d)
                    }
            }
            dL  <-  map_lgl (dL, ~ ! is.na (.) %>% all (.)) %>%  dL [.]    #  Kill the list element of NA and NULL;  dL <-  list (1:3, NA, 4, NULL, 5)
            if (length (naturalOrder) == 1 && naturalOrder == T && length (dL) > 1) {
                    dL  <-  names (dL)  %>%  naturalorder (.) %>%  dL [.]    #  Sort as the raw data
            } else if (is.numeric (naturalOrder)) {    #  Manually input the desirable order like c (3, 4, 1, 2)
                    if (length (natureOrder) == length (names (dL)))  dL  <-  dL [naturalOrder]
            }
            return (dL)
    }

    ##  HTML table  == (2019-11-12) ================================================
    html.  <-  function (dt, ... ) {
            if (is.atomic (dt) && ! is.null (names (dt))) dt  <-  as.list (dt)  %>%  list2tibble. (.)    #  Case with a vector with names created by sapply ()
            num2chr  <-  function (num, ... ) {
                    if (! is.numeric (num))  return (num)
                    Digits  <-  gsub ('\\.', '', num)  %>% gsub ('^0', '', .)  %>%  str_length (.)
                    Digits  <-  rep (3, length (num))    # mm unit allows 0.001;  '%.3f'
                    Digits [num >= 100]  <-  0    #  100.000 looks bothersome, so change it to 100
                    chr  <-  vector (mode = 'character', length = length (num))
                    for (i in seq_along (num))  chr [i]  <- sprintf (str_c ('%.', Digits [i], 'f'), num [i])
                    return (chr)
            }
            dt  <-  dt  %>%  mutate_if (~ is.numeric (.), ~ num2chr (.))    #  All data is character
            'DT' :: datatable (dt, rownames = F, options = list (pageLength = 100), filter = 'top')    #  %>%  'DT' :: formatRound (columns = names (dt), digits = 3)
    }

    ##  Quick chekc for basic statistics  == (2020-01-09) ================================================
    base_stats.  <-  function (d, ... ) {
            statsN  <-  c ('Median', 'Avg', 'SD', 'Max', 'Max without outliers', 'Min without outliers', 'Min', 'Range', 'Total', 'Number')
            if (is.list (d)) {
                    dt  <-  list2tibble. (d)  %>%  select_if (~ is.numeric (.) | is_time. (.) & n_distinct (.) > 1)    # No column with the same value
                    Stats  <-  rbind (median. (dt), mean. (dt), sd. (dt), max. (dt), max2. (dt, na = T), min2. (dt, na = T), min. (dt), delta. (dt), sum. (dt), length. (dt))
                    res  <-  cbind (Basic = statsN, Stats) %>%  as_tibble (.)  %>%  {skipMess. (type_convert (.))}
            } else if (is.atomic (d)) {
                    Stats  <-  c (median. (d), mean. (d), sd. (d), max. (d), max2. (d), min2. (d), min. (d), delta. (d), sum. (d), length. (d))
                    vecN  <-  substitute (d)  %>% as.character (.)  %>%  {if (length (.) == 1)  .  else  . [2]} #  Confirm;  substitute (iris [[1]]) %>% as.character
                    res  <-  bind_cols (Basic = statsN, x = Stats)  %>%  setNames (., c ('Basic', vecN))
            }
            return (res)
    }    #  base_stats. (iris)  %>%  html. (.)

    ##  Search for the nearest number of which the target vector is almost equal to the reference value  == (2020-01-23) ========================
    whichNear.  <-  function (vec, ref, back = F, ... )  map. (ref, ~ which (abs (vec -.) == min. (abs (vec -.))) %>% nth (., ifelse (back, -1, 1)) )

    ##  Select just value you want according to a condition (sorry for misleading name);  Note ref = sizeVec in length  == (2019-11-14) ============
#    whichSize.  <-  function (vec, ref, sizeVec, ... )  map. (vec, ~  whichNear. (., ref) %>% sizeVec [.])    #  cex = whichSize. (ncol (dt), c (4, 13, 30), c (0.6, 0.5, 0.4))
    whichSize.  <-  function (vec, ref, sizeVec, ... ) whichNear. (vec, ref)  %>%  sizeVec [.]    #  cex = whichSize. (ref = ncol (dt), vec = c (4, 13, 30), c (0.6, 0.5, 0.4))

    ##  Japanese or not for label & legend  == (2020-01-21) ================================================
    jL.  <-  function (chrs, ... ) {
            if (class (chrs) == 'character') {
                    if (! exists ('chrs') || is.null (chrs) || anyNA (chrs))  return (ifelse (Sys.getenv ('OS') == '', 'Avenir Next', 'sans'))
                    chr2  <-  str_replace_all (chrs, pattern = '[:blank:]|[:punct:]|\n', replacement = '')
                    tf  <-  str_match_all (chr2, pattern = '[:upper:]|[:lower:]|[:digit:]')  %>%  sapply (., nrow)  %>% {. != str_length (chr2)}
            } else {
                    return (ifelse (Sys.getenv ('OS') == '', 'Avenir Next', 'sans'))    #  in case of 'expression'
            }
            if (any. (tf)) {
                    ifelse (Sys.getenv ('OS') == '', 'HiraginoSans-W3', 'Meiryo')
            } else {
                    ifelse (Sys.getenv ('OS') == '', 'Avenir Next', 'sans')    #  Avenir  Ubuntu Open Sans  Light    #  names (pdfFonts ())
            }
    }    #  mtext (~, family = jL. (c (Xlab, Ylab)))

    ##  Correct abnormal Jap characters ================================================
    hankana2zenkana.  <-  function (chr, ... ) {
            if (! is.character (chr))  chr  <-  as.character (chr)
            ##  Converting semi-dakuten
            dh  <-  c ('ｶﾞ','ｷﾞ','ｸﾞ','ｹﾞ','ｺﾞ','ｻﾞ','ｼﾞ','ｽﾞ','ｾﾞ','ｿﾞ','ﾀﾞ','ﾁﾞ','ﾂﾞ','ﾃﾞ','ﾄﾞ','ﾊﾞ','ﾋﾞ','ﾌﾞ','ﾍﾞ','ﾎﾞ','ﾊﾟ','ﾋﾟ','ﾌﾟ','ﾍﾟ','ﾎﾟ')
            dz  <-  c ('ガ','ギ','グ','ゲ','ゴ','ザ','ジ','ズ','ゼ','ゾ','ダ','ヂ','ヅ','デ','ド','バ','ビ','ブ','ベ','ボ','パ','ピ','プ','ペ','ポ')
            for (i in seq_along (dz))  chr  <-  gsub (dh [i], dz [i], chr)
            ##  Converting full width space or double spaces to half one
            chr  <-  gsub ('　|  ', ' ', chr)
            ##  Converting 1bite character
            chr  <-  chartr ('ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ｡｢｣､･ｦｧｨｩｪｫｬｭｮｯｰ',
'アイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワヲン。「」、・ヲァィゥェォャュョッー',
                                 chr)
            return (chr)
    }

    ##  Zenkaku convert for Mac/Win  == (2019-11-14) ================================================
    zenk.  <-  function (chr, ... ) {
            chr  <-  chr  %>%  as.character (.)  %>%  gsub ('\r\n', '', .)  %>%  gsub ('  ', '', .)  %>%    #  gsub ('ー', '-', .)  #  Avoid killing '\n' for names of box2. ()
            {if (any. (validUTF8 (.)) || Sys.getenv ('OS') != '') .  else  iconv (., 'utf-8', 'cp932')}  %>%  hankana2zenkana. (.) %>%  map. (., ~ 'stringi' :: stri_trans_nfkc (.))
            return (chr)
    }

    ##  Reshape text  by cutting space & common characters ================================================
    neatChr.  <-  function (chr, ... ) {    #  c ('nya :: A', 'nya :: B') --> c ('A', 'B')
            ##  Delete space character
            regularChar  <-  c ('(', ')', '[', ']', '$', '^', '?', '*', '+', '{', '}', '|', '\'')    #  For starters, replace the regular chr that cannot allow to use str_detect ().
            for (i in seq_along (regularChar))  chr  <-  gsub (regularChar [i], ' ', chr, fixed = T)  %>%  str_trim (., side = 'both')
            if (length (chr) == 1)  return (chr)
            ##  Search for common characters and delete them.
            len  <-  min. (str_length (chr))
            ctr  <-  NULL
            for (i in 2: len) {    #  Why it starts 2 is; if a string is 'A' and it's removed, then the string after cutting will be ''.
                    tenta  <-  str_trunc (chr, width = i, ellipsis = '')  %>%  unique (.)
                    if (length (tenta) == 1 && str_detect (tenta, '[:alpha:]') %>% all (.))  ctr  <-  i
            }
            if (! is.null (ctr))  chr  <-  str_replace (chr, pattern = substr (chr [1], 1, ctr), replacement = '')
            return (chr)
    }

    ##  Interactive filter  == (2019-11-15) ================================================
    chooseOne.  <-  function (factors, messText = NULL, freqs = NULL, chr = T, ... ) {    #  freqs denotes each N of the factors, chr = T returns text (F does number).
            if (length (factors) == 1)  return (factors)
            tenta  <-  rep (NA_character_, length (factors))
            if (! is.null (freqs)) {
                    chrLen  <-  map. (factors, ~ nchar (., type = 'width'))
                    spaceLen  <-  max (chrLen) -chrLen +1
                    freqN  <-  map. (seq_along (tenta), ~ str_c (str_flatten (rep (' ', spaceLen [.])), '(N = ', freqs [.], ')') )
            } else {
                    freqN  <-  NULL
            }
            for (i in seq_along (tenta))  tenta [i]  <-  str_c ('     [', i, ']', ifelse (i < 10, '   ', '  '), factors [i], freqN [i], '\n')
            cat (paste0 ('\n      Choose one from below;\n\n'), tenta)
            repeat {
                    num  <-  readline (str_c ('\n      Which No.', ifelse (is.null (messText), '', 'as '), messText, ' ?  \n\n >>> '))
                    if (zenk. (num)  %>%  {skipMess. (as.numeric (.))}  %>%  {! is.na (.)} ) {
                            num  <-  zenk. (num)  %>% as.numeric (.)    #  To gurantee your input as numeric
                            if (num >= 1 && num <= length (factors))  break    #  This if () restricts proper range and prohibit minus or oversized.
                    }
            }
            cat (str_c ('\n', str_dup ('=', times = 50), '\n\n'))
            return (ifelse (chr, factors [num], num))    #  text or its number
    }
    #  fct  <-  map. (dt [, 1],  ~ unique (.) %>% naturalsort (., decreasing = F))
    #  freqs  <-  table (dt [, 1])  %>%  {. [naturalorder (names (.), decreasing = F)]}  %>%  as.numeric (.)
    #  dt  <-  chooseOne. (fct, '\"BOND\"', freqs = freqs) %>%  str_detect (dt $ 'Bond', pattern = .)  %>%  'dplyr' :: filter (dt, .)

    ##  Plot range for plot.window () & axisFun. () == (2020-01-21) ================================================
    pr.  <-  function (vec, XYlims = NA, expand_ratio = 0.02, ... ) {
            if ('list'  %in% class (vec))  vec  <-  unlist (vec)
            vec [which (vec == Inf | vec == -Inf)]  <-  NA
            def. (c ('Min', 'Max'), list (min. (vec), max. (vec)))    #  Type free for vec, list, data.frame
            ##  Type numbering;  complicated but needed to use each '&'  '&&' below...
            range_type  <-  XYlims  %>%  { c ( (length (.) > 1) & c (! anyNA (.), is.na (. [1]), is.na (. [2])),  is.na (.) && length (.) == 1) }  %>%  which (.)
            xyR  <-  list (XYlims, c (Min, XYlims [2]), c (XYlims [1], Max), c (Min, Max)) [[range_type]]
            expand_direction  <-  list (c (0, 0), c (-1, 0), c (0, 1), c (-1, 1)) [[range_type]]
            XYlim2  <-  xyR +diff (xyR) *expand_direction *expand_ratio
            return (XYlim2)
    }    #  pr. (vec = iris [, 1], XYlims = NA, 0.013)

    ##  Axis value  ================================================
    halfSeq.  <-  function (vec)  vec [-1] -diff (vec) /2    # Solution of bn = (an+1 - an)/2
    axisFun.  <-  function (XYlims, n = 5, ... )  pretty (XYlims, n = n)  %>%  list (mainTicks = ., subTicks = halfSeq. (.))

    ##  Cyclic number if it's over range for the interactive input == (2019-11-17) ================================================
    n_cyc.  <-  function (num, n_max, ... )  num %% n_max %>%  ifelse (. != 0, ., n_max)  %>%  return (.)

    ##  Creat translucent color  == (completely the same; adjustcolor (colors, tr, ...),  lol,  tr := transparency) ========================
    colTr.  <-  function (color, tr, ... )  if (is.null (color) || is.na (color) || color == 0)  '#FFFFFF00'  else  rgb (t (col2rgb (color) /255), max = 1, alpha = tr)

    ##  Color gradient with Y values  == (2019-11-14) ================================================
    colGra.  <-  function (vec, colors, ColorSteps = 13, ... ) {
            colors2  <-  c (colTr. (colors, tr = 0.6), 'grey80', colTr. (colors, tr = 0.6))    #  sapply (vec, abs)  %>%  max. (.)
            kingMax  <-  abs (vec)  %>%  max. (.)    #  NOTE: the 2nd para. of the following findInterval () are not {min, max} but {-max, +max}
            return (colorRampPalette (colors2) (ColorSteps) [findInterval (vec, seq (- kingMax, + kingMax, length.out = ColorSteps))])
    }

    ##  Auto color assignment  == (2020-01-20) ================================================
    colors.  <-  function (col = NA, d = NULL, ... ) {
            col  <-  col [! is.na (col)]    #  If col is NA, then turns into logical (0)
            col_base  <-  c ('black', 'cadetblue4', 'darkorange', 'darkseagreen3', 'firebrick3', 'hotpink2', 'peachpuff2', 'lightsalmon3', 'tomato2',
                                   'deeppink3')
            if (length (col) == 0 && is.null (d)) {
                    return (col_base)    #  colors. ()
            } else if (is.character (col)) {    #  This is for the plot function whose color argument you set
                    tenta  <-  ! col %in% colors ()
                    if (any. (tenta)) {
                            stop (str_c (col [tenta], collapse = ', ') %>% str_c ('Wrong color spell;  ', ., '\n\n'), call. = F) #  colors. (c ('red', 'parplu', 'pinc'))
                    } else {
                            col_supplemented  <-  if (is.null (d))  col  else  rep (col, times = n_factor. (d)) %>% {. [1: n_factor. (d)]}
                            return (col_supplemented)    #  colors. (c ('grey35', 'blue3'), iris)
                    }
            } else if (is.numeric (col)) {    #  Zero brings complexity;  except 0, numbers (even minius or large) change into col_base
                    col_num  <-  abs (floor (col))  %>% map_chr (., ~ {if (. == 0)  '#FFFFFF00'  else  n_cyc. (., length (col_base)) %>% col_base [.]} )
                    col_supplemented  <-  if (is.null (d)) col_num  else  rep (col_num, times = n_factor. (d)) %>% {. [1: n_factor. (d)]}
                    return (col_supplemented)    #  colors. (30:31, iris)
            } else if (length (col) == 0 && ! is.null (d)) {    #  Auto assignment according to data
                    if (n_factor. (d) < 13) {
                            return (col_base)    #  colors. (d = iris)    #  this is the same to colors. (), but is a kind of safety net...
                    } else {
                            col_many  <-  c ('A','B','C','D') [floor (runif (1, min = 1, max = 4 +1))]  %>%  {'viridisLite' :: viridis (n_factor. (d), option = .)}  #%>%  rev (.)
                            return (col_many)    #  colors. (d = c (iris, iris, iris))
                    }
            }
    }

    ##  Halo around text  == https://stackoverflow.com/questions/25631216/r-plots-is-there-any-way-to-draw-border-shadow-or-buffer-around-text-labels ==
    haloText.  <-  function (x, y, labels, cex, col = 'grey13', bg = 'white', theta = seq (0, 2 *pi, length.out = 36), r = 0.05, ... ) {
            'purrr' :: invoke_map (function (i)  text (x +r *strwidth ('A') *cos (i), y +r *strheight ('A') *sin (i), labels, cex = cex, col = bg, ... ), as.list (theta))
            text (x, y, labels, cex = cex *0.95, col = col, ... ) #  Draw actual text
    }

    ##  Optimum position of y-axis label  == (2020-01-21) ================================================
    yPos.  <-  function (Ylim2, ... )  axisFun. (Ylim2, n = 5) [[1]]  %>%  {. [between (., Ylim2 [1], Ylim2 [2])]}  %>% {strwidth (.) /strwidth ('|||')}  %>%
                                                  {max (ceiling (.))}  %>%  whichSize. (vec = c (1, 2, 3, 4), ref = ., c (1.6, 1.3, 1.00, 0.88))    #  same adjust to '-0.1' & '0.1' & '100'

    ##  legend  == (2019-11-09) ========================
    legeX.  <-  function (legePos_x, ... )  par ('usr') [1] +diff (par ('usr') [1:2]) *legePos_x
    legeY.  <-  function (legePos_y, ... )  par ('usr') [3] +diff (par ('usr') [3:4]) *legePos_y

## Now is the time == (2020-02-07) ================================================
today2. <- function(...) today(tz = 'Asia/Tokyo') %>% gsub('-', '', .) %>% str_sub(., 3, 8)
now2. <- function(...) now(tz = 'Asia/Tokyo') %>% gsub('-|:', '', .) %>% gsub (' ', '-', .) %>% str_sub(., 3, 13)


## Save graphics == (2020-02-07) ================================================
save. <- function(name = NULL, type = 'jpg', WH = dev.size(), ...) {
    saveN <- name %||% today2.()
    if (type %in% c('jpg', 'jpeg', 'j')) {
        dev.copy(jpeg, file = str_c(saveN, '.jpg'), units = 'in', width = WH[1], height = WH[2], res = 150)
        dev.off()
    }
    if (type %in% 'png') {
        dev.copy(png, file = str_c(saveN, '.png'), units = 'in', width = WH[1], height = WH[2], res = 350)
        dev.off()
    }
}
save2. <- function(name = NULL, WH = c(4.3, 3.3), ...) {
    saveN <- name %||% now2.()
    if (names(dev.cur()) == 'cario_pdf') dev.off()
    tryPDF <- function(...) try(skipMess.(cairo_pdf(str_c(saveN, '.pdf'), width = WH[1], height = WH[2], bg = 'transparent', onefile = T)), silent = T)
    if (class(tryPDF()) == 'try-error') {
        if (names(dev.cur()) != 'null device') dev.off()
        str_c('Do close \"', saveN, '.pdf\" on your application !!\n\n') %>% stop(., call. = F)
    } else {
        dev.off(); tryPDF()
    }
    gp.()
}


## Write list data to csv/xlsx file == (2019-02-11) ================================================
write. <- function(d, name = NULL, ...) {
    if ('list' %in% class(d)) d <- list2tibble.(d)
    name <- {name %||% today2.()} %>% {if (str_detect(., '\\.csv')) . else str_c (., '.csv')}
    write.csv(d, name, row.names = F, na = '', fileEncoding = 'cp932')
}
write2. <- function(dL, name = NULL, ...) {
    if (! 'list' %in% class(dL)) stop('Change the data into a list...\n\n', call. = F)
    if (is.null(names(dL))) names(dL) <- str_c('#', seq_along(dL))
    if (is.null(name)) name <- today2.()
    'openxlsx'::write.xlsx(dL, file = str_c(name, '.xlsx'))
}  # write2.(list(iris, mtcars, chickwts, quakes))


    ##  Fitting by GAM (Generized Additive Model) - GCV (Generized Cross Validation)  == (2019-01-31) ========================
    gamXY.  <-  function (x, y, mdlGet = F, boost = F, n.boost = NULL, ... ) {
            skipMess. (library ('mgcv'))
            x  <-  x [order (x)];  y  <-  y [order (x)]
            ##  Small data length cannot allow to use knot style.
            gamKnots  <-  function (knots, ...)  try (gam (y ~ s (x, k = knots, bs = 'cr'), method = 'REML'), silent = T)
            minGCV  <-  if (class (gamKnots (15)) [1] == 'try-error')  gamKnots (5)  else  gamKnots (15)    # if true, return; [1]  'gam' 'glm' 'lm'
            if (mdlGet == T)  return (minGCV)
            if (boost == T) {
                    qXlen  <-  if (is.null (n.boost))  whichSize. (length (x), c (50, 500), c (130, 888))  else  n.boost
                    qX  <-  seq (min. (x), max. (x), length = qXlen)
                    xyFit  <-  data.frame (x = qX, y = predict (minGCV, newdata = data.frame (x = qX)))
            } else if (boost == F) {    #  GAM model always ignores NA and you need to arrange. Even a plot with NA looks vacant, the fitted line shows proper position.
                    y [! is.na (y)]  <-  fitted (minGCV)    # Use NA info of original y and express unnatural appearance of vacant data
                    xyFit  <-  data.frame (x = x, y = y)
            }
            return (xyFit)
    }

    ##  Finding curve intersection; different (x, y) version;  not so accurate if df has not so many data points.  == (2020-01-23) ==
    ##  just vector but accurate analysis; https://stackoverflow.com/questions/20519431/finding-point-of-intersection-in-r
    ##  another Ref; https://stackoverflow.com/questions/31574382/intersection-between-density-plots-of-multiple-groups
    intersectX.  <-  function (df1, df2, ... ) {
            if (is.null (ncol (df1)) || is.null (ncol (df2)))  {df1 <- data.frame (x = seq_along (df1), y = df1);  df2 <- data.frame (x = seq_along (df2), y = df2)}    #  for vector
            s1  <-  rep (NA_real_, nrow (df1))
            for (i in seq_along (s1))  s1 [i]  <-  whichNear. (vec = df2 [[1]], ref = df1 [[i, 1]])  %>%  {abs (df1 [[i, 2]] - df2 [[., 2]])}
            s1  <-  scale (s1)  %>%  as.vector ()
            fp  <-  'pracma' :: findpeaks (-s1, nups = 10, ndowns = 10)    #  '-' is to search for valleys. nups & ndowns are stricts of successive peaks.
        #  plot (s1);  points (- fp [, 1] ~ fp [, 2], pch = 19, cex = 0.8, col = 'tomato2')    #  Valleys analysis needs '- fp [, 1]' for plot.
            crossX  <-  rep (NA_real_, nrow (fp))    #  Variation filter > 0.01
            for (i in seq_along (crossX))  crossX [i]  <-  fp [i, 2]  %>%  {abs (s1 [. +1] -s1 [.]) / ((. +1) - .)}  %>% {ifelse (. > 0.01, fp [i, 2], NA)}
            return (crossX [! is.na (crossX)]  %>%  df1 [., 1])
    }    #  plot (df1, type = 'l');  lines (df2, col = 'red'); abline (v = intersectX. (df1, df2))

    ##  Quick plot  == (2020-02-05) ================================================
    quickPlot.  <-  function (d, natural = F, Ltys = NA, Lwd = NA,  xlab = '', ylab = '', col = NA, Xlims = NA, Ylims = NA, legePos = NA, name = NULL, PDF = T,
                                       add = 0, mar = par ('mar'), tcl = par ('tcl'), ... ) {
            ##  You must prepare a data of list (tibble (x = ..., y = ...)) to draw x-y graph;  otherwise n-x & n-y graph are separately drawn.
            dL  <-  dLformer. (d, natural)
            if ('list' %in% class (dL [[1]]))  stop ('Try again with a data something like [ID, y] or [y1, y2, ...].\n\n', call. = F) #  Forbit [ID, y1, y2, ...] like iris data
            if (is.atomic (dL [[1]]))  dL  <-  map (dL, ~ tibble (x = seq_along (.), y = .))    #  if d isn't a list of xy set, convert index type.
            if (anyNA (Ltys) || length (Ltys) < length (dL)) Ltys  <-  Ltys [! is.na (Ltys)]  %>%  {c (., rep (1, times = length (dL) -length (.)))}
            if (is.na (Lwd))  Lwd  <-  whichSize. (length (dL), c (8, 25, 50), c (1.5, 0.8, 0.15))
            col  <-  colors. (col, d = max (length (dL), length (name)) %>% seq (.))
            def. (c ('Xlim2', 'Ylim2'), list (pr. (map. (dL, ~ . [1]), Xlims, 0.02), pr. (map. (dL, ~ . [2]), Ylims, 0.12) ))
            if (add != 2) {    #  add = 0;  normal plot,  add = 1; just prepare empty cambas,  add = 2;  add lines only
                    par (mar = mar, tcl = tcl)
                    plot.new ()    #  ex)  quickPlot. (d, add = 1); polygon (~);  quickPlot. (d, add = 2)
                    plot.window (xlim = Xlim2, ylim = Ylim2)
                    if (add == 1)  return ()
            }
            for (i in seq_along (dL))  lines (dL [[i]], lty = Ltys [i], col = colTr. (col [i], tr = 0.8), lwd = Lwd)
            for (i in 1:2) for (j in 1:2) {
                    axis (side = 2 *j -1, at = axisFun. (Xlim2, n = 6) [[i]], labels = (i *j == 1), tcl = par ('tcl') /i, cex.axis = 1, lend = 'butt', padj = -0.1)
                    axis (side = 2 *j, at = axisFun. (Ylim2, n = 6) [[i]], labels = (i *j == 1), tcl = par ('tcl') /i, cex.axis = 1, lend = 'butt')
            }
            box ()
            mtext (xlab, side = 1, las = 1, cex = 1, family = jL. (xlab), line = par ('mar') [1] -1.00)
            mtext (ylab, side = 2, las = 3, cex = 1, family = jL. (ylab), line = par ('mar') [2] -yPos. (Ylim2))
            if (length (dL) != 1 || ! is.null (name) && ! 0 %in% name) {    #  No legend is needed for one line at least. Or name = 0 will realize no legend
                    if (is.null (name))  name  <-  names (dL) %>%  {if (! is.null (.))  .  else  str_c ('#', seq_along (dL))}    #  Auto assignment
                    par (family = jL. (name))
                    nameLen  <-  'stringi' :: stri_numbytes (name)  %>%  max. (.)    #  Count including multi bytes char and space
                    legePosX  <-  if (! anyNA (legePos))  legePos [1]  else  nameLen *(-0.01) +0.70
                    legePosY  <-  if (! anyNA (legePos) && length (legePos) == 2)  legePos [2]  else  0.975
                    Cex  <-  whichSize. (ref = nameLen, vec = c (15, 33, 38, 45, 54), c (0.85, 0.65, 0.58, 0.49, 0.55))  %>%
                                 {. *whichSize. (ref = length (dL), vec = c (5, 10, 20), c (1, 0.8, 0.6))}
                    yInt  <-  whichSize. (ref = nameLen, vec = c (15, 33, 38, 45, 54), c (1.3, 1.4, 1.5, 1.6, 1.4))  %>%
                                 {. *whichSize. (ref = length (dL), vec = c (5, 10, 20), c (1, 1.1, 1.2))}
                    legend (x = legeX. (legePosX), y = legeY. (legePosY), legend = name, x.intersp = 0.65, y.intersp = yInt, cex = Cex,
                               horiz = F, box.lty = 0, col = colTr. (col, tr = 0.8), text.col = col, lwd = 0.95, seg.len = 1.3, lty = Ltys, bg = NULL)    #  'white'  NULL
                    par (family = ifelse (Sys.getenv ('OS') == '', 'Avenir Next', 'sans'))
            }
            if (names (dev.cur ()) == 'cairo_pdf' && PDF == T)  skipMess. (dev.off ())
     }    #  quickPlot. (iris)  quickPlot. (iris [, -5], legePos = c (0.01, 0.99))  quickPlot. (psd [[1]])  quickPlot. (list (psd [, 2:3]), Ylims = c (0, NA))

    ##  Kernel Density Estimation plot  == (2020-01-10) ================================================
    dens.  <-  function (d, bw = 1, natural = F, Ltys = NA, Lwd = NA, xlab = '', ylab = '', col = NA, Xlims = NA, Ylims = c (0, NA), legePos = NA, name = NULL,
                                 refcol = 1, areaPer = 0.95, tail_right = NULL, tail = NULL, ... ) {
            kde_xy  <-  function (vec, ... ) {
                    Bw  <-  list ('nrd0', 'Sj-ste', (4/3) ^ (1/5) * sd. (vec) * length. (vec) ^ (-1/5))  %>%  . [[bw]]
                    Dens  <-  density (vec, na.rm = T, bw = Bw, n = 1300)
                    if (min. (vec) >= 0 && any. (Dens $ 'x' < 0)) {
                            for (i in seq (1, 0.5, by = -0.005)) {
                                    Dens  <-  density (vec, na.rm = T, bw = Bw, n = 1300, adjust = i)    #  bw  <-  adjust * bw
                                    if (all (Dens $ 'x' >= 0)) break
                            }
                            if (any. (Dens $ 'x' < 0)) {
                                    for (i in seq (1, 0.5, by = -0.005)) {    #  Sometimes warning;  'Auto-range choice cut-off at 0'
                                            Dens  <-  skipMess. ('logKDE' :: logdensity (vec, na.rm = T, bw = Bw, n = 1300, adjust = i))    #  bw  <-  adjust * bw
                                            if (all (Dens $ 'x' >= 0))  break
                                    }
                            }
                    }
                    return (tibble (x = Dens $ 'x', y = Dens $ 'y'))
            }
            dL  <-  dLformer. (d, natural)  %>%  {if (is.atomic (. [[1]]))  map (., ~ kde_xy (.))  else  stop ('Only available for [ID, y] or [y1, y2, ...]', call. = F)}
            quickPlot. (dL, natural, Ltys, Lwd,  xlab, ylab, col, Xlims, Ylims, legePos, name)

            ##  Caluculate a consistency for REF's kernel density, comparing each area in the same x range
            if (is.null (names (dL)))  names (dL)  <-  str_c ('#', seq_along (dL))
            refcol  <-   n_cyc. (refcol, n_factor. (dL))
            ##  Cumulative range (percentage)
            def. (c ('Larea', 'Rarea'), list ((1 -areaPer) /2, (1 +areaPer) /2 ))
            def. (c ('tail_right', 'tail_max'), list (tail_right %||% 0.95, 1))
            ref_cdf  <-  dL [[refcol]]  %>%  {cumP. (. $ 'y')}    #  Cumulative Density Function
            LRx  <-  whichNear. (vec = ref_cdf, ref = c (Larea, Rarea))  %>%  dL [[refcol]] $ 'x' [.]
            R2x  <-  whichNear. (vec = ref_cdf, ref = c (tail_right, tail_max))  %>%  dL [[refcol]] $ 'x' [.]
            ##  Actural value range (real number)
            Tail  <-  if (is.null (tail)) {    #  Right tail to max
                    R2x
            } else {
                    if (length (tail) == 1) {
                            whichNear. (dL [[refcol]] $ 'x', tail) %>%  dL [[refcol]] [[., 1]]  %>%  c (., R2x [2])    #  'Over x' probability
                    } else {
                            if (length (tail) != 1)  whichNear. (vec = dL [[refcol]] $ 'x', ref = c (tail [1], tail [2]))  %>%  dL [[refcol]] $ 'x' [.]
                    }
            }
            ##  Tail area calculations
            area_LRx  <-  sapply (dL, area_part., LRx = LRx) #  Each area in the same x range
#          area_LRX  <-  area_LRx [refcol]  %>%  {(1 -abs (area_LRx -.) /.) *100}  %>%  round (., 2)
            area_R2x  <-  sapply (dL, area_part., LRx = R2x)
            area_Tail  <-  sapply (dL, area_part., LRx = Tail)
            names_area_df  <-  tibble (c (str_c ('Total area: ', areaPer *100, '%'), str_c ('Right tail area: D', tail_right *100, ' to D100'),
                                                       str_c ('Real values: ', sprintf ('%.1f', Tail [1]), ' to ', sprintf ('%.1f', Tail [2])) ))  %>%  setNames ('Area Type')
            dens_out  <<-  bind_rows (area_LRx, area_R2x, area_Tail)  %>%  as_tibble (.)  %>%  bind_cols (names_area_df, .)    #  A safety if you want the result
            names (dens_out) [1 +refcol]  <-  str_c (names (dens_out) [1 +refcol], ' (REF)')
            cat ('\n\n\"Probability-like\" consistency is the following...\n\n');  print (dens_out);  cat ('\n\n')
    }    #  dens. (iris)  # NG;  dens. (iris [, 4:5])  # [ID, y] type;  dens. (iris [, 3:4], refcol = 2, tail = c (1.5, 2))  # Prob. calc;

    ##  Histograms plot  == (2020-01-20) ================================================
    hist.  <-  function (d, naturalOrder = F, binW = 1, freq = T, xlab = '', ylab = '', col = NA, Xlims = NA, ... ) {    #  NOTE: Xlims also works to cut data range into Xlims.
            ##  Make the bin width (if vec is only integer, the ticks are positioned in the center of each bar)
            whatBreak  <-  function (vec, ... ) {
                    if (binW %in% c ('St', 'st', 'Sturges') || all (abs (vec) <= 1))  return ('Sturges')
                    if (binW %in% c ('Sc', 'sc', 'Scott'))  return ('Scott')
                    if (all (vec %% 1 == 0, na.rm = T))  {(min. (vec) -0.5) : (max. (vec) +0.5)}  %>%  return (.)    #  dL <-  floor (runif (100, 0, 30))
                    if (! is.na (binW))  {c (floor (min. (vec)), ceiling (max. (vec)))}  %>%  {seq (. [1], . [2], by = binW)} %>%  return (.)
            }
            dL  <-  dLformer. (d, naturalOrder)    #  list of vectors, not xy.
            ##  NOTE:  Screening the range you wanna observe.
            if (length (Xlims) > 1 && anyNA (Xlims)) { #  When Xlims = c (0, NA)
                    dL  <-  map (dL, ~ . %>%  {if (is.na (Xlims [1]))  . [. <= Xlims [2]]  else  . [. >= Xlims [1]]})
            } else if (length (Xlims) > 1 && ! anyNA (Xlims)) {    #  When Xlims = c (0, 100)
                    dL  <-  map (dL, ~ . [. >= Xlims [1] &. <= Xlims [2]])
            }
            Xlim2  <-  pr. (vec = dL, NA, 0.025)
            tenta  <-  vector ()
            for (i in seq_along (dL)) {
                    tenta [i]  <-  dL [[i]]  %>%  . [! is.na (.)]  %>%  hist (., breaks = whatBreak (.), plot = F)  %>% {if (freq)  . $ 'counts'  else  . $ 'density'}  %>%  max (.)
                    if (i == length (dL))  Ymax  <-  max. (tenta)
            }
            Ylim2  <-  pr. (vec = c (0, Ymax), c (0, NA), 0.08)
            par (mgp = c (0, 0.4, 0))
            for (i in seq_along (dL)) {
                    vec  <-  dL [[i]]  %>%  . [! is.na (.)]
                    hist (vec, ann = F, axes = F, freq = freq, xlim = Xlim2, ylim = Ylim2, col = colTr. (col [i], 0.80), breaks = whatBreak (vec))
                    for (i in 1:2) {
                            axis (side = 1, at = axisFun. (Xlim2, n = 5) [[i]], labels = (i == 1), tcl = -par ('tcl') /i, cex.axis = 1, lend = 'butt', padj = -0.25)
                            axis (side = 2, at = axisFun. (Ylim2, n = 5) [[i]], labels = (i == 1), tcl = -par ('tcl') /i, cex.axis = 1, lend = 'butt')
                    }
                    box (bty = 'l')
                    mtext (xlab, side = 1, las = 1, line = par ('mar') [1] -1.00, cex = 1, family = jL. (xlab))
                    {if (ylab == '' && freq)  'Frequency' else if (ylab == '' && ! freq)  'Density'  else  ylab} %>%
                    mtext (., side = 2, las = 3, line = par ('mar') [2] -yPos. (Ylim2) +0.15, cex = 1, family = jL. (ylab))
            }
            if (names (dev.cur ()) == 'cairo_pdf')  skipMess. (dev.off ())
    }

    ##  Pie chart for ratio  == (2019-08-20) ================================================
    pie.  <-  function (d, ... ) {
            dL  <-  dLformer. (d)    #  Desirable for the data.frame with only ONE column
            pie (table (dL), col = NULL, border = 'grey13', clockwise = T, init.angle = 90, radius = 0.8, cex = 0.9)
            par (new = T)
            pie (table (dL), label = '', col = 'white', border = 'white', radius = 0.5)
            Theta  <-  seq (- pi, pi, length = 350)
            lines    (0.5 * cos (Theta), 0.5 * sin (Theta), col = 'grey13')
            text (0, 0, labels = names (dL), family = jL. (names (dL)))
    }

    ##  Linear correlation plot  == (2020-01-20) ================================================
    ##  NOTE.1  Regression analysis is strictly applicable to cause (x) and effect (y; random variable) on the assumption that x has no error...
    ##  NOTE.2  Correlation analysis is regarded as both x & y random variables;  thus don't use linear regression and probability ellipse at the same time...
    ##  Trivia.1  You'll see the cross points on the line and ellipse can draw y-axis parallel lines as tangent.
    ##  Trivia.2  The longer axis of the ellipse is corresponding to the axis of principal component analysis.
    corp.  <-  function (dt, xlab = '', ylab = '', col = 5, legePos = NULL, li = F, el = T, ... ) {    #  xy data
            if (is.character (col))  stop ('Don\'t use color name like \'blue\'.  Use numbers 1 to 6, thank you.\n\n', call. = F)
            colpal  <-  n_cyc. (col, 6)  %>%  c ('Greys', 'Blues', 'Oranges', 'Purples', 'Reds', 'Greens') [.]
            if (ncol (dt) != 2)  stop ('Make sure the data only consists 2-xy columns...\n\n', call. = F)
            dt  <-  dt  %>%  'dplyr' :: filter (rowSums (is.na (.)) == 0)    #  Omit the row including any NA
            def. (c ('x', 'y'), list (dt [[1]], dt [[2]]))
            ## 'elliplot' :: ellipseplot (iris [c (5, 1)], iris [c (5, 2)])
            mdl0  <-  'robustbase' :: lmrob (y ~x -1, na.action = na.exclude, setting = 'KS2014')    #  'robust' :: lmRob (y ~ x -1, na.action = na.exclude)
            mdl1  <-  'robustbase' :: lmrob (y ~x +1, na.action = na.exclude, setting = 'KS2014')    #  'robust' :: lmRob (y ~ x +1, na.action = na.exclude)
            mdlNum  <-  map. (list (mdl0, mdl1), ~ summary (.) %>% . $ 'sigma')  %>%  which.min (.)
            mdl  <-  list (mdl0, mdl1) [[mdlNum]]    #  Choose better
            Coef  <-  list (c (0, coef (mdl0)), coef (mdl1)) [[mdlNum]]  %>%  setNames (NULL)
            ##  Cor  <-  'robust' :: covRob (dt, corr = T) $ 'cov' [1, 2]    #  No robust, including outliers;  cor.test (x, y, method = 'pearson') $ 'estimate'
            ##  Cnt  <-  'robust' :: covRob (dt, corr = T) $ 'center'    #  Note: If Coef [2] ~ ±0.01 and shows strong Cor, don't care because it's 1to1 relationship.
            Cor  <-  if (nrow (dt) > 13)  'robustbase' :: covMcd (dt, cor = T) $ 'cor' [1, 2]  else  cor (dt) [1, 2]    # covMcd results are somtimes different for small data.
            Cnt  <-  'robustbase' :: covMcd (dt, cor = T) $ 'center'    #  c (mean. (x), Coef [1] +Coef [2] *mean. (x))

            ##  Legend position 1, 2, 3, 4 as quadrant
            text_pos  <-  function (...) {
                    xyPos  <-  {Cnt -c (mean (par ('usr') [1:2]), mean (par ('usr') [3:4]))}  %>%  {. >= c (0, 0)}
                    def. (c ('xpos', 'ypos', 'yScMin', 'yScMax'), list (xyPos [1], xyPos [2], par ('usr') [3], par ('usr') [4] ))
                    cnt_pos  <-  if (xpos && ypos)  1 else if (! xpos && ypos)  2  else if (! xpos && ! ypos)  3  else if (xpos && ! ypos)  4
                    list (
                            if (Coef [2] >= 0 && Coef [1] < yScMin)  2  else if (Coef [2] >= 0 && Coef [1] >= yScMin)  4  else if (Coef [2] < 0)  3,
                            if (Coef [2] >= 0)  4  else if (Coef [2] < 0 && Coef [1] < yScMax)  1  else if (Coef [2] < 0 && Coef [1] >= yScMax)  3,
                            if (Coef [2] >= 0 && Coef [1] < yScMin)  2  else if (Coef [2] >= 0 && Coef [1] >= yScMin)  4  else if (Coef [2] < 0)  1,
                            if (Coef [2] >= 0)  2  else if (Coef [2] < 0 && Coef [1] < yScMax)  1  else if (Coef [2] < 0 && Coef [1] >= yScMax)  3
                    ) [[cnt_pos]]  %>%  return (.)
            }
            ##  Legend equation
            text3  <-  function (text_pos, ... ) {
                    Show  <-  function (x)  ifelse (any. (Cnt > 10), 1, 2)  %>%  {sprintf (str_c ('%.', ., 'f'), x)} %>%  return (.)
                    Text1  <-  bquote ('(' *bar (italic (x)) *',' ~bar (italic (y)) *')' == '(' *.(Show (mean. (x))) *','  ~.(Show (mean. (y))) *')')
                    Text2  <-  Coef [1]  %>%  {c (. > 0, . < 0, . == 0)}  %>%  which (.)  %>%  list (
                                           bquote (hat (italic (y)) == .(sprintf ('%.2f', Coef [2])) *italic (x) + .(sprintf ('%.2f', Coef [1])) *phantom (')')),
                                           bquote (hat (italic (y)) == .(sprintf ('%.2f', Coef [2])) *italic (x) ~ .(sprintf ('%.2f', Coef [1])) *phantom (')')),
                                           bquote (hat (italic (y)) == .(sprintf ('%.2f', Coef [2])) *italic (x) *phantom (')'))
                                   ) [[.]]
                    Text3  <-  bquote (italic (R) [adj] ^2  == .(sprintf ('%.2f', summary (mdl) $ 'adj.r.squared')) )
                    State  <-  function (x)  if (abs (x) < 0.25) 'none' else if (abs (x) < 0.50) 'weak' else if (abs (x) < 0.75) 'moderate' else 'strong'
                    Text4  <-  bquote (italic (r) [Pearson]  == .(sprintf ('%.2f', Cor)) ~ (.(State (Cor))) )   # No 'Perfect' due to robust cut
                    Text5  <-  if (cor.test (x, y) $ 'p.value' > 0.004)  bquote ('p-value'  == .(sprintf ('%.2f', cor.test (x, y) $ 'p.value')) )  else  bquote ('p-value ~ 0.00')
                    ##  Set proper text postion
                    lege4  <-  list (c (0.98, 1.00), c (0.05, 1.00), c (0.05, 0.32), c (0.98, 0.32))
                    if (is.null (legePos))  legePos  <-  lege4 [[text_pos]]
                    text_num  <-  if (li && el)  1:5 else if (li && ! el)  1:3  else if (! li && el)  c (1, 4, 5)  else if (! li && ! el)  1
                    for (i in seq_along (text_num)) {
                            text (legeX. (legePos [1]), legeY. (legePos [2] -0.08 *i), adj = ifelse (legePos [1] < 0.5, 0, 1), cex = 0.8, col = '#22222295',
                                    label = list (Text1, Text2, Text3, Text4, Text5) [[text_num [i] ]],
                                    family = ifelse (Sys.getenv ('OS') == '', 'CenturySchoolbook', 'Times New Roman'))
                    }
            }
            ## http://friendly.github.io/heplots/reference/covEllipses.html
            ##  'heplots' :: covEllipses (dt, col = colTr. ('grey35', 0.8), lwd = 1, level = 0.95, labels = '', center.pch = '', method = 'mcd', add = T)    #  'mve'
            draw_ellipse  <-  function (...) {    #  Minimum Covariance Determinant (MCD)
                    elli95  <-  {if (nrow (dt) > 13) 'robustbase' :: covMcd (dt) $ 'cov'  else  cov (dt)}  %>%
                                   'ellipse' :: ellipse (., centre = Cnt, level = 0.95, npoints = 200)  %>%  as_tibble (.)
                    lines (elli95, col = colTr. ('black', 0.35), lwd = 1.0)
            }
            ##  Drawing
            if (between (min. (x) /min. (y), 0.9, 1.1) & between (max. (x) /max. (y), 0.9, 1.1) ) {    #  When x and y are slightly different;  keep the same scale
                   def. (c ('Xlim2', 'Ylim2'), list (pr. (c (x, y), NA, 0.13), pr. (c (x, y), NA, 0.13)) )
            } else {    #  When x and y are largely different; change proper scale to see easily
                   def. (c ('Xlim2', 'Ylim2'), list (pr. (x, NA, 0.13), pr. (y, NA, 0.13)) )
            }
            par (mgp = c (0, 0.4, 0))
            plot.new ()
            plot.window (xlim = Xlim2, ylim = Ylim2)
            Colcol  <-  if (nrow (dt) >= 20) {
                                    densCols (x, y, colramp = colorRampPalette (c ('grey90', 'RColorBrewer' :: brewer.pal (9, colpal))))
                           } else {
                                    str_sub (colpal, start = -1, end = -1)  <-  ''    #  Remove the last 's' like 'Greys' --> 'Grey'
                                    colpal  %>%  tolower (.) %>%  colTr. (., 0.35)    #  Sigle coloring for small data
                           }
            points (x, y, lwd = 0.95,  pch = 19, cex = 1.3, col = Colcol)
            if (li == T)  abline (mdl, col = '#22222221', lwd = 3.5)    #  KS2014 line
            if (el == T)  draw_ellipse ()    #  Modified ecllipse
            for (i in 1:2) {
                    axis (side = 1, at = axisFun. (Xlim2, n = 4) [[i]], labels = (i == 1), tcl = -par ('tcl') /i, cex.axis = 1, lend = 'butt', padj = -0.2)
                    axis (side = 2, at = axisFun. (Ylim2, n = 4) [[i]], labels = (i == 1), tcl = -par ('tcl') /i, cex.axis = 1, lend = 'butt')
            }
            box (bty = 'l')
            mtext (xlab, side = 1, las = 1, cex = 1, family = jL. (xlab), line = par ('mar') [1] -1.00)
            mtext (ylab, side = 2, las = 3, cex = 1, family = jL. (ylab), line = par ('mar') [2] -yPos. (Ylim2))
            textP  <-  text_pos ()
            text3 (textP)
            if (names (dev.cur ()) == 'cairo_pdf')  skipMess. (dev.off ())
    }    # END of corp. ()  corp. (iris [, 3:4])

    ##  Boxplot oriented for quantile limit and full/half box  == (2020-01-20) ================================================
    boxplot2.  <-  function (dL, type, jit, val, wid, Ylims, col, name, xlab, ylab, rot, ... ) {
            ##  Scale of half or full boxplot
            if (type == 'full' || type == 'f')  def. (c ('AT', 'jitW', 'leftW', 'rightW'), list (0, wid *0.6, wid, wid))
            if (type != 'full' && type != 'f')  def. (c ('AT', 'jitW', 'leftW', 'rightW'), list (wid /2, wid /2, wid, 0))
            xPos  <-  2 *seq (length (dL)) -1    #  NA is already omitted
            CX  <-  length. (dL)  %>%  max. (.)  %>% whichSize. (., c (100, 13, 4), c (0.3, 0.7, 0.8))
            ##  fivenum () is agreed with quantile () if vec is odd, but if even, fivenum () is a bit wider than quantile ().
            ##  Moreover, some cases make wrong whiskers that have no points more or less than 95th or 5th by quantile ().
            c1  <-  map. (dL, function (x)  {fivenum (x) [2] -1.5 *IQR (x, na.rm = T)}  %>%  c (., min. (x))  %>%  max. (.))
            c2  <-  map. (dL, ~ fivenum (.) [2])
            c3  <-  map. (dL, ~ fivenum (.) [3])
            c4  <-  map. (dL, ~ fivenum (.) [4])
            c5  <-  map. (dL, function (x)  {fivenum (x) [4] +1.5 *IQR (x, na.rm = T)}  %>%  c (., max. (x))  %>%  min. (.))
            ##  for loop is needed because outlier isn't always just one ...
            points_outliers  <-  function (...) {
                    stripC  <-  function (vec, pos, pch, bg, col, ... ) {
                            stripchart (vec, at = pos +AT, vertical = T, method = 'jitter', jitter = jitW, add = T, lwd = 0.3, pch = pch, bg = bg, cex = CX,
                                            col = if_else (col == '#FFFFFF00', 'grey13', col) )
                    }
                    for (i in seq_along (dL)) {
                            vec  <-  dL [[i]]
                            Yin  <-  vec [vec >= c1 [i] & vec <= c5 [i]]
                            Yout  <-  vec [! vec %in% Yin]
                            if (jit == T) {
                                    stripC (Yin, xPos [i], 21, bg = colTr. (col [i], 0.55), col = col [i])
                                    stripC (Yout, xPos [i], 4, colTr. ('grey13', 0.8), col = col [i])
                            } else {
                                    points (rep (xPos [i], length (Yout)), Yout,  lwd = 0.3, cex = CX)
                            }
                    }
            }
            ##  Scale of half or full boxplot
            box_whiskers  <-  function (...) {
                    rect (xPos -leftW, c2, xPos +rightW, c4, border = 0, col = colTr. (col, 0.55))
                    segments (xPos -leftW, c3, xPos +rightW, c3, col = colTr. ('grey13', 0.95), lwd = 3, lend = 'butt')
                    rect (xPos -leftW, c2, xPos +rightW, c4, border = colTr. ('grey13', 0.95))
                    segments (xPos -leftW /2, c1, xPos +rightW/2, c1, col = colTr. ('grey13', 0.95))
                    segments (xPos -leftW/2, c5, xPos +rightW/2, c5, col = colTr. ('grey13', 0.95))
                    segments (xPos, c1, xPos, c2, lty = 'dashed', col = colTr. ('grey13', 0.95))
                    segments (xPos, c4, xPos, c5, lty = 'dashed', col = colTr. ('grey13', 0.95))
            }
            ##  Show values
            textFun  <-  function (...) {
                    Digit  <-  unlist (dL)  %>%  delta. (.) %>%  {if (. < 1 && all (median. (dL) < 1))  3 else  whichSize. (., c (50, 5, 1), c (0, 1, 2)) }
                    tCex  <-  whichSize. (length (dL), c (4, 13, 30), c (0.6, 0.5, 0.4))
                    dD  <-  map (dL, quantile, probs = c (0, 0.5, 1), na.rm = T)  %>%  data.frame (.)
                    for (i in seq_along (dD)) {
                            if (abs (round (dD [1, i] -dD [2, i], Digit +1)) < 0.7 *10 ^(-Digit))  dD [1, i]  <-  NA
                            if (abs (round (dD [3, i] -dD [2, i], Digit +1)) < 0.7 *10 ^(-Digit))  dD [3, i]  <-  NA
                    }
                #  haloText. (xPos +AT, dD [2, ], labels = sprintf (str_c ('%.', Digit, 'f'), dD [2, ]), cex = tCex *1.5)    #  Too slow ...
                    text (xPos +AT, dD [2, ], labels = sprintf (str_c ('%.', Digit, 'f'), dD [2, ]), col = 'white', cex = tCex *1.5 *1.05)    #  Alternative for haloText ...
                    text (xPos +AT, dD [2, ], labels = sprintf (str_c ('%.', Digit, 'f'), dD [2, ]), col = 'grey13', cex = tCex *1.5 *0.95)    #  Slightly easy to look ...
                    text (xPos +AT, dD [1, ], labels = sprintf (str_c ('%.', Digit, 'f'), dD [1, ]), col = 'grey70', cex = tCex, adj = c (0.5, 1.8))
                    text (xPos +AT, dD [3, ], labels = sprintf (str_c ('%.', Digit, 'f'), dD [3, ]), col = 'grey70', cex = tCex, adj = c (0.5, -1.0))
            }
            ##  base plot
            par (mgp = c (0, 0.4, 0))
            if (length (dL) > 30)  par (mar = c (4, 3.3, 0.1, 1.0))
            Xlim2  <-  c (-1, 2 *length (dL) +1) +wid *c (1, -1)
            Ylim2  <-  pr. (dL, Ylims, 0.07)    #  NOTE:  text of Max or Min is not shown by 0.05
            plot.new ()
            plot.window (xlim = Xlim2, ylim = Ylim2)
            abline (v = par ('usr') [1], h = par ('usr') [3], col = 'grey13', lwd = 2 *par ('lwd'))    #  needed twice normal lwd
            axis (1, at = xPos, labels = F, lwd.ticks = par ('lwd'), tcl = -par ('tcl'), cex.axis = 1, lend = 'butt')
            for (i in seq (2))  axis (2, at = axisFun. (Ylim2, n = 5) [[i]], labels = (i == 1), lwd.ticks = par ('lwd'), tcl = -par ('tcl') /i, cex.axis = 1, lend = 'butt')
            mtext (xlab, side = 1, las = 1, cex = whichSize. (nchar (xlab), c (15, 35, 50), c (1, 0.8, 0.5)), family = jL. (xlab), line = par ('mar') [1] -1.00)
            mtext (ylab, side = 2, las = 3, cex = whichSize. (nchar (ylab), c (15, 35, 50), c (1, 0.8, 0.5)), family = jL. (ylab), line = par ('mar') [2] -yPos. (Ylim2))
            ##  x-label
            labAdj  <-  function (name, ... ) {
                    Line  <-  map. (str_count (name, '\n'), ~ whichSize. (., c (0, 1, 2), c (0.3, 1.3, 1.3)))  %>%
                                 {. *whichSize. (length (dL), c (8, 15, 35, 60, 100), c (1, 1, 1, 1.5, 2))}
                    Cex  <-  map. (str_count (name, '\n'), ~ whichSize. (., c (0, 1, 2), c (0.90, 0.85, 0.6)))  %>%
                                 {. *whichSize. (length (dL), c (8, 15, 35, 60), c (1, 0.8, 0.6, 0.37))}
                    return (list (line = Line, cex = Cex))
            }
            name  <-  if (! is.null (name))  name  else {if (is.null (names (dL)))  str_c ('#', seq_along (dL))  else  names (dL) }  %>%
                           gsub ('\\n', '\n', ., fixed = T)  %>% zenk. (.)
            if (length (name) < length (xPos))  name  <-  c (name, rep (NA_character_, times = length (xPos) - length (name)))
            if (rot == 0) {
                    mtext (name, at = xPos, side = 1, las = 1, cex = labAdj (name) $ 'cex', family = jL. (name), line = labAdj (name) $ 'line')
            } else {
                    yPos  <-  par ('usr') [3:4]  %>%  delta. (.)  %>%  {par ('usr') [3] -0.025 *. *whichSize. (length (dL), c (8, 15, 35, 60), c (0.9, 0.8, 0.7, 0.6))}
                    nameLen  <-  'stringi' :: stri_numbytes (name)  %>%  max. (.)    #  Count including multi bytes char and space
                    rot_cex  <-  whichSize. (nameLen, c (5, 10, 15), c (0.8, 0.7, 0.6))  %>%  {. *whichSize. (length (dL), c (8, 15, 35, 60), c (0.9, 0.8, 0.7, 0.6))}
                    text (xPos, yPos, name, srt = rot,  xpd = T, adj = c (1, 1), cex = rot_cex, family = jL. (name))
            }
            ##  boxplot
            points_outliers ()
            box_whiskers ()
            if (val == T)  textFun ()
    }
    box2.  <-  function (d, type = 'half', jit = T, val = T, natural = F, wid = 0.75, Ylims = NA, col = 0, name = NULL, xlab = '', ylab = '', rot = 0, cut = F, sel = NULL, med_order = F, name_marking = NULL, col_marking = NULL, PDF = T, ... ) {
            dL  <-  dLformer. (d, natural)    #  you can control an order something like  'OK' 'NG'  by sel = ~
            if (cut == T) {
                    for (i in seq_along (dL)) {
                            vec  <-  dL [[i]]
                            outs  <-  quantile (vec, probs = c (0.25, 0.75), na.rm = T) +c (-1, 1) *IQR (vec, na.rm = T) *3.0    # NOTE the last term is the cut-off criteria
                            vec [which (vec < outs [1] | vec > outs [2] )]  <-  NA    #  Delete too large or small outliers
                            dL [[i]]  <-  vec
                    }
            }
            if (! is.null (sel))  dL  <-  n_cyc. (sel)  %>% dL [.]    #  try  as.list (iris) [0: 100]  %>%  dLformer. (.)
            if (med_order == T)  dL  <-  median. (dL)  %>% order (., decreasing = T)  %>%  dL [.]    #  Show the graph like Pareto chart
            col  <-  colors. (col, d = dL)
            if (! is.null (name_marking)) {    #  name_marking = list (c ('Log-Normal', 'Nukiyama-Tanasawa', 'Three-parameter Log-Hyperbolic', 'Weibull'))
                    for (i in seq_along (col_marking))  col [which (names (dL) %in% name_marking [[i]])]  <-  col_marking [[i]] #  col_marking = list ('grey88')
            }
            ##  Signle or multiple boxplot
            if (! 'list' %in% class (dL [[1]]) ) {    #  type:  [ID, y] or [y1, y2, y3, ...]
                    if (map_lgl (dL, ~ is.numeric (.)) %>% any (.) %>% `!`)  stop ('The data does NOT any numeric data...\n\n', call. = F)
                    if (ylab == '' && is.data.frame (d) && select_if (d, ~ ! is.numeric (.)) %>% {ncol (.) == 1})  ylab  <-  map_lgl (d, ~ is.numeric (.))  %>%  names (d) [.]
                    boxplot2. (dL, type, jit, val, wid, Ylims, col, name, xlab, ylab, rot)
            } else {    #  type:  [ID, y1, y2, y3, ...]
                    for (i in seq_along (dL))  boxplot2. (dL [[i]], type, jit, val, wid, Ylims, col, name, xlab, ylab = names (dL [i]), rot)
            }
            if (names (dev.cur ()) == 'cairo_pdf' && PDF == T)  skipMess. (dev.off ())
    }    #  box2. (iris [, -5], col = 1:4, rot = 22, cut = T)  box2. (iris)

    ##  Scatter plot marix  == (2020-01-20) ========================
    sp.  <-  function (d, col = NULL, xlab = '', ylab = '', cut = F, conv = T, ... ) {    #  (conv = T) means normalization of all data
            dt  <-  list2tibble. (d)
            if (cut == T) {
                    dt  <-  mutate_if (dt, ~ is.numeric (.), function (vec, ... ) {
                            outs  <-  quantile (vec, probs = c (0.25, 0.75), na.rm = T) +c (-1, 1) *IQR (vec, na.rm = T) *3.0    # NOTE the last term is the cut-off criteria
                            vec [which (vec < outs [1] | vec > outs [2] )]  <-  NA    #  Delete too large or small outliers
                            return (vec)
                    })
            }
            okngCol_TF  <-  dt  %>%  select_if (~ is.character (.))  %>%  map_lgl (., ~ str_detect (., pattern = 'OK|NG') %>% any. (.))
            resultCol  <-  which (okngCol_TF)  %>%  names (.) %>%  str_subset (., pattern = '結果|Result|result')  %>%  . [1]    #  If failed, return NA
            dt  <-  dt  %>%  select_if (~ n_distinct (.) > 1)    #  sd. (.) > 0;  sd = 0 causes error;  delete somthing like machine number
            if (! is.na (resultCol)) {
                    Results  <-  map. (dt [[resultCol]], function (x)  if (x %in% 'OK')  1  else if (x %in% 'NG')  0  else  NA)
                    dt  <-  tibble (Results)  %>%  bind_cols (., dt)
            }
            if (conv)  dt  <-  mutate_if (dt, ~ is.numeric (.), ~ scale. (.))
            if (is.character (col))  stop ('The argument \"col\" must be numeric...\n\n', call. = F)
            cols  <-  list (c ('grey88', 'grey55', 'grey35'), c ('darkorange1', 'darkorange4', 'darkorange'), c ('springgreen1', 'springgreen4', 'seagreen3'),
                                c ('dodgerblue1', 'dodgerblue4', 'skyblue3'), c ('tomato1', 'tomato4', 'red1'))  %>%  {if (is.null (col))  c (0, 0, 0)  else  . [[n_cyc. (col, 5)]]}
            par (family = jL. (names (dt)))
            'psych'  ::  pairs.panels (dt, smooth = T, scale = T, density = T, ellipses = T, stars = T, lm = T, method = 'spearman', cex.cor = 1.3, cex.labels = 0.8, gap = 0.3,
                       col = cols [1], pch = 21, bg = colTr. (cols [2], 0.5), hist.col = colTr. (cols [3], 0.8), cex.axis = ifelse (par () $ 'family' == 'Avenir Next', 1, 0.95)
            )
            mtext (xlab, side = 1, las = 1, cex = 1, family = jL. (xlab), outer = T, line = par ('mar') [1] -1.00)
            mtext (ylab, side = 2, las = 3, cex = 1, family = jL. (ylab), outer = T, line = par ('mar') [2] -0.88)
            par (family = ifelse (Sys.getenv ('OS') == '', 'Avenir Next', 'sans'))
            if (names (dev.cur ()) == 'cairo_pdf')  skipMess. (dev.off ())
    }    #  save. ('nya', type = 'jpg', WH = c (4.3, 3.3) *1.8)

    ##  matplot2  == (2020-01-20) ================================================
    mat2.  <-  function (dt, xlab = NULL, ylab = NULL, ... ) { #  matplot cannot draw a missing line across NA part.
            cols  <-  c ('grey13', 'springgreen3', 'tomato2', 'dodgerblue3', 'maroon3', rep ('grey13', 10))
            def. (c ('x', 'dty', 'Xlims', 'Ylims'), list (dt [[1]], dt [, -1], range. (dt [[1]]), unlist (dt [, -1]) %>% range. (.)) )
            plot.new ()
            plot.window (xlim = pr. (Xlims, c (-1, 1), 0.03), ylim = pr. (Ylims, c (-1, 1), 0.08))
            for (i in seq_along (dty))  data.frame (x, dty [[i]]) %>%  na.omit (.)  %>%  lines (., col = cols [i])
            for (i in seq_along (dty))  data.frame (x, dty [[i]]) %>%  na.omit (.)  %>%  points (., pch = 21, col = 'grey13', bg = colTr. (cols [i], 0.75))
            for (i in 1:2) {
                    axis (side = 1, at = axisFun. (Xlims, n = 5) [[i]], labels = (i == 1), tcl = par ('tcl') /i, lend = 'butt', padj = -0.2)
                    axis (side = 2, at = axisFun. (Ylims, n = 5) [[i]], labels = (i == 1), tcl = par ('tcl') /i, lend = 'butt')
            }
            box ()
            mtext (xlab, side = 1, las = 1, cex = 1, family = jL. (xlab), line = par ('mar') [1] -1.00)
            mtext (ylab, side = 2, las = 3, cex = 1, family = jL. (ylab), line = par ('mar') [2] -yPos. (Ylims))
            if (names (dev.cur ()) == 'cairo_pdf')  skipMess. (dev.off ())
    }

    ##  Multiple definition  == (2019-01-10) ================================================
    def.  <-  function (defnames, values)  for (i in seq_along (defnames))  assign (defnames [i], values [[i]], envir = parent.frame ())
#  def. (c ('cat', 'run'), list (35, 1:23))

    ##  map () returns vectors  == (2019-01-09) ================================================
    map.  <-  function (.x, .f, ... )  'purrr' :: map (.x, .f, ... )  %>%  unlist (., use.names = F)

    ##  Signature function for math treatment  == (2020-01-05) ================================================
    sgn.  <-  function (x)  case_when (x > 0 ~ 1, x == 0 ~ 0, x < 0 ~ -1)

    ##  Rescaling (min-max normalization) for math treatment  == (2020-01-05) ================================================
    rescaling.  <-  function (x)  (x -min. (x)) / (max. (x) -min. (x))

    ##  Short cut to kill bothersome etc.  == (2020-01-09) ================================================
    pmax.  <-  function (x)  pmax (x, na.rm = T)
    pmin.  <-  function (x)  pmin (x, na.rm = T)
    scale.  <-  function (x)  scale (x, center = min. (x), scale = delta. (x))    #  Normalization for 0-1;  Y = (y -y_min) /range
    any.  <-  function (x)  as.logical (x)  %>%  any (., na.rm = T)
    ymd.  <-  function (x)  if (is.POSIXct (x))  floor_date (x, 'day') %>% as.character (.) %>% gsub (' JST', '', .)  else  x
    range0.  <-  function (x)  if (is.atomic (x) && is.numeric (x))  range (x, na.rm = T, finite = F)  else  NA
    range.  <-  function (x)  if (is.atomic (x))  range0. (x) else  list2tibble. (x) %>% select_if (~ is.numeric (.)) %>% unlist (.) %>% range0. (.)
    median0.  <-  function (x)  if (is.atomic (x) && is.numeric (x) || is_time. (x))  median (x, na.rm = T) %>% ymd. (.)  else  NA
    median.  <-  function (x)  if (is.atomic (x))  median0. (x) else if (is.list (x)) {
                      list2tibble. (x) %>% select_if (~ is.numeric (.) | is_time. (.)) %>% sapply (., median0.) }  else NA
    mean0.  <-  function (x, trim = 0)  if (is.atomic (x) && is.numeric (x) || is_time. (x))  mean (x, na.rm = T, trim) %>% ymd. (.)  else  NA
    mean.  <-  function (x, trim = 0)  if (is.atomic (x))  mean0. (x, trim)  else if (is.list (x)) {
                      list2tibble. (x) %>% select_if (~ is.numeric (.) | is_time. (.)) %>% sapply (., mean0., trim) } else  NA
    sd0.  <-  function (x)  if (is.atomic (x) && is.numeric (x))  sd (x, na.rm = T)  else  NA
    sd.  <-  function (x)  if (is.atomic (x))  sd0. (x)  else if (is.list (x)) {
                      list2tibble. (x) %>% select_if (~ is.numeric (.) | is_time. (.)) %>% sapply (., sd0.) }  else  NA
    var0.  <-  function (x)  if (is.atomic (x) && is.numeric (x))  var (x, y = NULL, na.rm = T)  else  NA
    var.  <-  function (x)  if (is.atomic (x))  var0. (x)  else if (is.list (x)) {
                      list2tibble. (x) %>% select_if (~ is.numeric (.) | is_time. (.)) %>% sapply (., var0.) }  else  NA
    max0.  <-  function (x, Nth = 1)  if (is.atomic (x) && is.numeric (x) || is_time. (x))  sort (x, decreasing = T) %>% . [Nth] %>% ymd. (.)  else  NA
    max.  <-  function (x, Nth = 1)  if (is.atomic (x))  max0. (x, Nth)  else if (is.list (x)) {
                      list2tibble. (x) %>% select_if (~ is.numeric (.) | is_time. (.)) %>% sapply (., max0., Nth) } else  NA
    min0.  <-  function (x, Nth = 1)  if (is.atomic (x) && is.numeric (x) || is_time. (x))  sort (x, decreasing = F) %>% . [Nth] %>% ymd. (.)  else  NA
    min.  <-  function (x, Nth = 1)  if (is.atomic (x))  min0. (x, Nth)  else if (is.list (x)) {
                      list2tibble. (x) %>% select_if (~ is.numeric (.) | is_time. (.)) %>% sapply (., min0., Nth) } else  NA
    max2_base.  <-  function (x, na) {
            if (! is.atomic (x) || ! is.numeric (x))  return (NA)
            def. (c ('whisker', 'Max'),  list (quantile (x, probs = 0.75, na.rm = T) +IQR (x, na.rm = T) *1.5, max (x, na.rm = T)) )
            {if (na == T && setequal (whisker, Max))  NA else  min (c (whisker, Max), na.rm = T)}  %>%  return (.)
    }
    max2.  <-  function (x, na = F)  if (is.atomic (x)) max2_base. (x, na)  else if (is.list (x)) {
                      list2tibble. (x) %>% select_if (~ is.numeric (.) | is_time. (.)) %>% sapply (., max2_base., na) } else  NA
    min2_base.  <-  function (x, na) {
            if (! is.atomic (x) || ! is.numeric (x))  return (NA)
            def. (c ('whisker', 'Min'),  list (quantile (x, probs = 0.25, na.rm = T) -IQR (x, na.rm = T) *1.5, min (x, na.rm = T)) )
            {if (na == T && setequal (whisker, Min))  NA else  max (c (whisker, Min), na.rm = T)}  %>%  return (.)
    }
    min2.  <-  function (x, na = F)  if (is.atomic (x)) min2_base. (x, na)  else if (is.list (x)) {
                      list2tibble. (x) %>% select_if (~ is.numeric (.) | is_time. (.)) %>% sapply (., min2_base., na) } else  NA
    which.max0.  <-  function (x, Nth = 1:1)  if (is.atomic (x) && is.numeric (x) || is_time. (x))  max. (x, Nth)  %>% {map. (., ~ which (x == .))}
    which.max.  <-  function (x, Nth = 1)  if (is.atomic (x)) which.max0. (x, Nth)  else if (is.list (x)) {    #  which.max0. (c (9, NA, 8:1), 1:2)  which.max0. (c (1,1,1))
                      list2tibble. (x) %>% select_if (~ is.numeric (.) | is_time. (.)) %>% sapply (., which.max0., Nth) }  else  NA
    which.min0.  <-  function (x, Nth = 1:1)  if (is.atomic (x) && is.numeric (x) || is_time. (x))  min. (x, Nth)  %>% {map. (., ~ which (x == .))}
    which.min.  <-  function (x, Nth = 1)  if (is.atomic (x)) which.min0. (x, Nth)  else if (is.list (x)) {    #  which.min. (c (3,-10,5,-88), 1:2)
                      list2tibble. (x) %>% select_if (~ is.numeric (.) | is_time. (.)) %>% sapply (., which.min0., Nth) }  else  NA
    delta0.  <-  function (x, unit = 'day')  if (is.atomic (x) && is.numeric (x)) { range (x, na.rm = T) %>% diff (.) %>% as.numeric (.)
                      }  else if (is.atomic (x) && is_time. (x)) {range (x, na.rm = T) %>% diff (.) %>% time_length (., unit = unit) %>% as.numeric (.)  } else  NA
    delta.  <-  function (x, unit = 'day')  if (is.atomic (x)) delta0. (x, unit)  else if (is.list (x)) {
                      list2tibble. (x) %>% select_if (~ is.numeric (.) | is_time. (.)) %>% sapply (., delta0., unit) } else  NA
    sum0.  <-  function (x)  if (is.atomic (x) && is.numeric (x) || is.logical (x))  sum (x, na.rm = T)  else  NA #  Note;  sum function eats T/F
    sum.  <-  function (x)  if (is.atomic (x))  sum0. (x)  else if (is.list (x)) {
                      list2tibble. (x) %>% select_if (~ is.numeric (.) | is_time. (.) | is.logical (.)) %>% sapply (., sum0.) }  else  NA
    length0.  <-  function (x)  if (is.atomic (x))  x [! is.na (x)] %>% length (.)  else  NA
    length.  <-  function (x)  if (is.atomic (x))  length0. (x) else if (is.list (x))  sapply (x, length0.)  else  NA    # Forbitten for length. (list (NULL))
    n_factor.  <-  function (x)  if (is.atomic (x))  length0. (x)  else if (is.data.frame (x))  ncol (x)  else if ('list' %in% class (x))  length (x)  else  NA

    ##  Area of polygon (widely applicable to any polygon with x order like convex-hull)  == (2019-12-17) ========================
    area.  <-  function (x, y, ... ) {    #  Newton-Cotes formulae (to only area surrounding x-axis and a curve):  sum (0.5 *diff (x) * (y [-1] +y [-length (y)]))
            if (length (x) != length (y))  stop ('x and y do not have the same length.\n\n', call. = F)
            def. (c ('x2', 'y2'), list (c (x [-1], x [1]), c (y [-1], y [1])))    #  [x1, ... , xn] --> [x2, ... , xn, x1]
            calc  <-  abs (sum (x *y2 -x2 *y)) /2    #  psd data;  polygon type = 1.004, Newton type = 1
            return (calc)
    }

    ##  Partial Area (Only applicable for PDF;  whose cuve is surrounding x axis)  == (2020-01-23) ========================
    area_part.  <-  function (dt, LRx, ... ) {    #  dt is PDF, LRx is partial range of x
            rowRange  <-  whichNear. (dt $ 'x', c (LRx [1], LRx [2]))  %>%  {. [1] : . [2]}
            def. (c ('x', 'y'), list (dt $ 'x' [RowRange], dt $ 'y' [RowRange]))
            calc  <-  sum (0.5 *diff (x) * (y [-1] +y [-length (y)]))
            return (calc)
    }

    ##  Cumulative probablity denstiy  == (2020-02-04) ========================
    cumP.  <-  function (y, ... )  cumsum (y) /sum (y)    # cumsum (0.5 *diff (x) * (y [-1] +y [-length (y)]))  %>%  c (0, .)    #  Note:  the later is over 0.6%...

    ##  AIC calculation  == (2019-05-11) ================================================
    aic.  <-  function (model, ... )  if (is.null (model))  NA else  model  %>%  {-2 *logLik (., REML = F) [1] +2 *(length (coef (.)) +1L)}
    aic2.  <-  function (model, ... ) {
            if (is.null (model))  return (NA)
            qy  <-  fitted (model)  %>%  . [. > 0]    # Sometimes a model returns minus fitted values partially and then results in NaN or Inf.
            return ( -1 /length (qy) *sum (log (qy)) +(length (coef (model)) +1L) /length (qy) )
    }

    ##  Other Information Criteria (OIC ?) calculation  == (2019-12-17) ================================================
    rmse.  <-  function (model, ... )  if (is.null (model))  NA else  model %>% {sqrt (deviance (.) /(length (fitted (.)) -length (coef (.))))}  #  = summary (model) $ sigma
    rmse2.  <-  function (model, ... )  if (is.null (model))  NA else  model %>% {log (2 *pi *deviance (.) /length (fitted (.))) +1}    #  Deviance
    rmse3.  <-  function (model, ... )  if (is.null (model))  NA else  model %>% {deviance (.) /sum ((fitted (.)) ^2)}    # Estimated error

    ##  Return with no error-stop risk?  == (2019-01-25) ================================================
    tryReturn.  <-  function (modeling)  suppressWarnings (try (modeling, silent = T)  %>%  {if (class (.) == 'try-error')  NA else  .} )
    #  tryReturn. (nlsLM (y ~ x, start = ... ))

    ##  Find the interval points on both sides of local plus/minus change in vector  == (2019-05-23) ========================
    interval2.  <-  function (vec, valley = T, ... ) {    #  True means a "single" local valley
            vecTF  <-  diff (vec)  %>%  {if (valley)  . >= 0  else  . < 0}    #  Search for a valley or peak
            if (sum (vecTF) %in% c (0, length (vecTF))) {
                    if (valley == T && sum (vecTF) == 0) return (length (vec))    #  Find a valley at the end
                    if (valley == T && sum (vecTF) == length (vecTF))  return (1)    #  Find a valley at the start
                    if (valley == F && sum (vecTF) == 0) return (length (vec))    #  Find a peak at the start
                    if (valley == F && sum (vecTF) == length (vecTF))  return (1)    #  Find a peak at the end
            } else {    #  2 Numbers on the change point
                    if (valley == T) {
                            vecTF  %>%  diff (.)  %>%  {which (. == 1) [1]}  %>%  {c (., . +2)}  %>%  {if (anyNA (.)) which.min (vec) [1]  else  .}  %>%  return (.)
                    } else {
                            vecTF  %>%  diff (.)  %>%  {which (. == 1) [1]}  %>%  {c (., . +2)}  %>%  {if (anyNA (.)) which.max (vec) [1]  else  .}  %>%  return (.)
                    }
            }
    }    #  interval2. (vec = c (3,2,1,2,3), T); interval2. (vec = c (4:1), T)

    ##  Find sequencial vector list, dividing sequential numbers into small groups  == (2019-01-14) ========================
    seqCtr.  <-  function (hit, Seq = 1, ... ) {    #  'hit' is the target number out of the rule, and 'magicSeq' is the sequence number.
            if (all (is.na (hit)))  return (NA)
            hit  <-  hit [! is.na (hit)]    #  hit  <-  c (1,2,9,10,11,14,17,18,19,22,40,41,42)
            dummy  <-  nth (hit, 1) : nth (hit, -1)    #  NOTE: both of the initial and last 'dummy' always are not NA.
            dummy [! dummy %in% hit]  <-  NA
            def. (c ('grp', 'grpList', 'j'), list (vector (), list (), 1))
            for (i in seq_along (dummy)) {
                    if (! is.na (dummy [i]))  grp  <-  c (grp, dummy [i])
                    if (is.na (dummy [i]) && ! is.logical (grp) || i == length (dummy)) {
                            grpList [[j]]  <-  grp
                            grp  <-  vector ()
                            j  <-  j +1
                    }
            }
            grpList  <-  {lapply (grpList, length) >= Seq} %>%  grpList [.]    #  Cropping larger than 'Seq'
            if (length (grpList) == 0)  grpList  <-  NA    #  In case that grpList = list () because of too large magicSeq or none of sequance.
            return (grpList)
    }    #  which (hit > 15) %>% seqCtr. (., Seq = 5)  := trueList. (hit > 15) %>% {. [map. (., ~ length (.) > 5)]}

    ##  Pick up only true vector and return their list ================================================
    trueList.  <-  function (tfVec, ... ) {    #  'tfVec' is constituted of TRUE or FALSE and its length is the same to the original vec.
            ctr  <-  vector ();  ctrList  <-  1;  grpList <-  list ()
            for (i in seq_along (tfVec)) {
                    if (tfVec [i] == T) {
                            ctr  <-  c (ctr, i)
                            if (i == length (tfVec)) {
                                    grpList [[ctrList]]  <-  ctr
                            }
                    } else if (i > 1 && tfVec [i-1] == T && tfVec [i] == F) {
                            grpList [[ctrList]]  <-  ctr
                            ctrList  <-  ctrList +1
                            ctr  <-  vector ()
                    }
            }
            return (grpList)    #  Return the true element number and make use of it
    }

    ##  Fast Anomaly Detection  == (2020-01-11) ========================
    cFilter.  <-  function (vec, shaper = 5, ... ) {    #  More odd shaper, more vivid change
            scaler1  <-  length (vec) ^shaper *exp ((mean. (vec) /sd. (vec)) ^(-1) )    #  Keep uniform for vector length;  note to use of coefficient of variation
            scaler2  <-  10 ^(-2 *shaper) *2 ^((shaper -3) /2) #  Keep uniform for many shapers
            vec  %>%  scale (.)  %>%  {cumsum (.) /length (.)}  %>%  {-. ^shaper}  %>%  {. *scaler1 *scaler2}
    }
    fad.  <-  function (vec, shaper = 3, ... ) {    #  Strongly recommended shaper = 3
            scaler1  <-  (1 *length (vec)) ^shaper *exp ((mean. (vec) /sd. (vec)) ^(-1) )    #  "
            scaler2  <-  10 ^(-2 *shaper) *2 ^((shaper -3) /2) #  "
            balancer  <-  c (0, 0, diff (vec, differences = 2) ^shaper)  %>%  scale (.)    #  Improvement
            vec  %>%  scale (.)  %>%  {cumsum (.) /length (.)}  %>%  {-. ^shaper}  %>%  {. *scaler1 *scaler2 *balancer}    #  Watch the last term
    }
#  vec  <-  psd [[1]];  plot (vec, type = 'l', lwd = 0.8);  lines ( cpDetecter. (vec), type = 'l', col = 'yellow')
#  par (new = T);  plot (cFilter. (vec), type = 'l', lwd = 0.8, col = 'darkseagreen', axes = F);  axis (4, col.axis = 'darkseagreen')
#  par (new = T);  plot (fad. (vec), type = 'l', lwd = 0.8, col = 'blue', axes = F)

    ##  Change points detection  == (2019-12-13) ================================================
    cpDetecter.  <-  function (vec, Lper = 0.05, entryRate = 0.25, gapRate = 0.60, ... ) {
    ##  https://qiita.com/hoxo_m/items/1afa288178422fad9076
    ## https://speakerdeck.com/hoxom/bi-nu-nizhen-rarenaitamefalsebian-hua-jian-zhi-ru-men
    ##  https://www.slideshare.net/siroyui/20160924-r-57
    ##  PELT (Pruned Exact Linear Time) is the fastest & most precise to seek several change points.
    ##  CROPS (Change points for a Range Of PenaltieS) makes decision of optimized change points. Without it, there produce too many points.
    ##  The original cpt program can detect even tiny change but industrial trend requires a kind of big change so that anyone could recognize it as cpt.
    ##  NOTE:  changepoint package doesn't follow NA action.
    ##  NOTE : 'minseglen' := minmum of cut segment length: Kill neighbors in the same large deviation
            vec0  <-  vec    #  vec0 (original) --> vec (no NA) --> vec1 (mean bars in the whole span)
            if (anyNA (vec))  vec  <-  vec0 [! is.na (vec0)]
            if (length (vec) < 10)  {cat (paste ('\n    CAUTION !!  Element length of the object is too short,', length (vec), '\n'));  return (NULL)}
            skipMess. (suppressPackageStartupMessages (library ('changepoint')))
            penValues  <-  c (5, 1000)    #  Very significant
            Minseg  <-  ifelse (length (vec) < 100, 5, ceiling (length (vec) * Lper))    #  Any time it's larger than 5. Try Lper (length %).
            invisible (capture.output (    #  cpt.meanvar () produces a model but also crap output: Needed invisible (cap ~)
                    craps  <-  cpt.meanvar (vec, method = 'PELT', penalty = 'CROPS', pen.value = penValues, minseglen = Minseg)
            ))
            ##  1)  List up x position of cpts with some penalty
            ten0  <-  craps  %>%  cpts.full (.)  %>% as.data.frame (.)    #  NOTE:  6 is my limit numer of. cpts
            selectCptRow  <-  pmap (ten0, ~ is.na (c (...)) %>% {ncol (ten0) - sum (.)})  %>%  {which (. <= 6)} %>%  {
                                               if (length (.) != 0) { {min. (.) : nrow (ten0) }
                                               }  else  {1: nrow (ten0)}
                                       }
            ten1  <-  ten0 [selectCptRow, ]  %>% as.data.frame (.)    #  NOTE:  if fully deleted it will change into integer.
            players  <-  unlist (ten1)  %>%  table (.)
            players [seq (players)]  <-  0    # Once reset zero
            if (length (players) > 0) {
                    for (i in seq (nrow (ten1))) {for (j in seq (ncol (ten1))) {
                            if (! is.na (ten0 [i, j])) {
                                    ijPos  <-  which (names (players)  %>%  parse_number (.)  == ten1 [i, j])
                                    players [ijPos]  <-  players [ijPos] + pen.value.full (craps) [selectCptRow] [i]
                            }
                    }}    #  This method always leave 1 cpt at least.
            }
            players  <-  players /max. (players)    #  plot (vec, type = 'l');  abline (v = names (players) %>% parse_number (.))
            players  <-  players [players > entryRate]    # TRY AGAIN  #  plot (vec, type = 'l');  abline (v = names (players) %>% parse_number (.))
            ##  plot (craps, cpt.width = 1.5, cpt.col = 'blue', ncpts = whichSize. (length (vec), c (30, 80, 150, 300), c (1, 3, 3, 6)))
            ##  ten0
            ##  NOTE:  cpts' candidates in ten0 are placed at NOT the start but the END of avg lines.
            ##  pen.value.full (craps)
            ##  plot (craps, diagnostic = T);  lines (seq (pen.value.full (craps)) -1, rev (pen.value.full (craps)), col = 'blue', lwd = 3, lty = 3)

            ##  source (file.path ('~/Library/Mobile Documents/com~apple~CloudDocs/R_script/SCCD', 'SPC_cpts_vec_example.R'), chdir = F)
            ##  entryRate of VEC1 (length = 288, Minseg = 15); 'shoulder' = 0.17 (6 cpts), 'elbow' = 0.56 (2 cpts), 'biceps' = 0.44 (3 cpts = entryRate smaller 0.37)
            ##  entryRate of VEC2 (length = 31, Minseg = 5); 'shoulder' = none, 'elbow' = 1.0 (1 cpt), 'biceps' = 0.38 (2 cpts = entryRate smaller 0.37, but it's faint)
            ##  entryRate of VEC3 (length = 158, Minseg = 8); 'shoulder' = - (8 cpts), 'elbow' = 1.0 (3 cpt), 'biceps' = 0.46 (6 cpts)  <-- so set 0.50 filter

            ##  2)  Check how vivid the changes are.
            gap  <-  gapRate *sd. (vec)    #  Very touchable on you observer for its stepwise appearance
            xcpt  <-  names (players)  %>%  as.numeric (.) %>%  c (1, ., length (vec))
            finalSelect1  <-  function (...) {    #  Updating with re-calculation of avg
                    if (length (xcpt) > 2) {
                            j  <-  1;  while (j <= 3) {    # Difference situation will change once any points vanish.
                                    muSeg  <-  rep (NA_real_, length (xcpt))
                                    for (i in seq_along (muSeg) [-1])  muSeg [i-1]  <-  mean. (vec [xcpt [i-1] : xcpt [i] ])
                                    bigPoint  <-  which (abs (diff (muSeg)) >= gap)  %>%  {xcpt [. +1]}
                                    xcpt  <-  c (1, bigPoint, length (vec))
                                    j  <-  j +1
                            }
                            if (length (xcpt) > 2) {    #  Kill a change point with tiny band
                                    del_tiny_Xwidth  <-  which (diff (xcpt) <= 2)  %>%  c (., . +1)  %>%  unique (.)
                                    if (length (del_tiny_Xwidth) != 0)  xcpt  <-  xcpt [-del_tiny_Xwidth]
                            }
                    };  return (xcpt)
            }
            finalSelect2  <-  function (...) {    #  No considering update with re-calculation of avg
                    if (length (xcpt) > 2) {
                            muSeg  <-  rep (NA_real_, length (xcpt) -1)
                            for (i in seq_along (muSeg))  muSeg [i] <-  mean. (vec [xcpt [i] : xcpt [i+1] ])
                            bigPoint  <-  diff (muSeg)  %>% {which (abs (.) >= gap)}  %>%  {xcpt [. +1]}
                            xcpt  <-  c (1, bigPoint, length (vec))
                    };  return (xcpt)
            }
            xcpt  <-  finalSelect1 ()    #  plot (vec, type = 'l');  abline (v = finalSelect1 ())
        #  xcpt  <-  finalSelect2 ()    #  plot (vec, type = 'l');  abline (v = finalSelect2 ())
            ##  3)  Assign all the element numbers on each of mean Yi or NA
            ##  Prepare avgs' vector including NA
            xseg  <-  {xcpt [- c (1, length (xcpt))] +1}  %>% c (xcpt, .)  %>%  sort (.)  %>%  matrix (., ncol = 2, byrow = T)    #  Shift 1 for segmentation
            vec1  <-  rep (NA_real_, nrow (xseg));  for (i in seq_along (vec1))  vec1 [ xseg [i, 1] : xseg [i, 2] ]  <-  mean. (vec [ xseg [i, 1] : xseg [i, 2] ])
            vec0 [! is.na (vec0)]  <-  vec1    #  At this moment, vec0 has been rewritten to avg info with NAs.
            ##  Insert 2 medium points at each of xcpts. This is just a gimmic so that the cpt graph looks stepwise at cpt points.
            xcpt2  <-  xcpt [- c (1, length (xcpt))]    #  If you skip NA, then lines () draws a single line. But the procedure below plots multiple lines in the NA space.
            num  <-  vec0    #  xcpt2 is based on vec without NA, and needs adjusted to numbering with NA inserting.
            num [! is.na (num)]  <-  which (! is.na (num)) %>%  seq (.)
            dtCpt  <-  tibble (num = num, x = seq (vec0), y = vec0)    #  x is changing but num keeps original element numbers.
            for (i in seq_along (xcpt2)) {
                    iRow  <-  which (dtCpt $ 'num' == xcpt2 [i])   #  Considered a data frame after i-th inserting
                    xInsert2  <-  iRow  %>%  dtCpt [., 'x'] %>%  pull (.)  %>%  {rep (. +0.5, times = 2)}    #  It's very confusing...
                    yInsert2  <-  dtCpt [iRow: (iRow +1), 'y'] %>%  pull (.)
                    dtCpt  <-  add_row (dtCpt, num = NA, x = xInsert2, y = yInsert2, .after = iRow)
            }
            df  <-  data.frame (select (dtCpt, - num))    # NOTE:  lines () cannot work with tibble style.
            return (df)    #  In case of no change point, it returns just a all avg line.
        #  plot (vec, type = 'l'); lines (df, lwd = 3.5, col = 'dodgerblue3')
    }

    ##  My built-in data sets  == (2019-11-12) ========================
    psd  <-  'tibble' :: tibble (
            D50_trend = c (5.9204,6.0121,6.0586,6.2209,6.2324,6.144,6.0984,6.2555,6.1247,5.9782,6.0046,5.9996,5.9208,5.9528,6.0332,5.9793,5.9767,5.9431,5.9328,5.9785,6.0054,5.9444,5.9229,5.9215,6.22,5.9157,6.2892,6.2653,6.3006,6.2087,6.22,6.1921,6.3196,6.1654,6.2312,6.2655,6.4325,6.2903,6.2603,6.2758,6.337,6.4528,6.2582,6.2155,6.2913,6.3557,6.2648,6.3149,6.0616,6.0744,6.0624,6.0622,6.0762,6.0595,6.0653,6.0408,6.0442,6.075,6.275,6.31,6.141,6.0529,6.1227,6.0991,6.0622,6.072,6.0536,6.1487,6.1659,6.1418,6.1735,6.198,6.1747,6.1977,6.0544,6.0561,6.0651,6.0939,6.1481,6.0275,6.1194,6.1463,6.0986,6.0979,6.085,6.1609,6.1578,6.1761,6.1789,6.063,6.0775,6.1347,6.1275,6.1043,6.1276,6.1167,6.0781,6.0724,5.4042,5.4122,5.47,5.4295,5.3981,5.4427,5.4199,5.4353,5.4154,5.4884,5.3858,5.5541,5.7985,5.8028,5.7826,5.7677,5.4786,5.431,5.4415,5.7419,5.6217,5.6355,5.7736,5.8625,5.6887,5.4465,5.453,5.7984,5.4901,5.5239,5.6456,5.596,5.5668,5.3313,5.5945,5.6021,5.8624,5.9725,5.7487,5.849,5.9329,5.9187,5.9747,6.0058,5.7944,5.8814,5.9312,5.9728,5.9993,5.9644,5.8304,5.7989,5.767,5.832,5.8286,5.7543,5.3395,5.2984,5.8302,5.616,5.7719,5.7628,5.4615,5.6009,5.5825,5.4694,5.7711,5.4174,5.635,5.6684,5.4135,5.846),
            x = c (4.52,4.69189349112426,4.86378698224852,5.03568047337278,5.20757396449704,5.3794674556213,5.55136094674556,5.72325443786982,5.89514792899408,6.06704142011834,6.2389349112426,6.41082840236686,6.58272189349112,6.75461538461538,6.92650887573964,7.0984023668639,7.27029585798816,7.44218934911243,7.61408284023669,7.78597633136095,7.95786982248521,8.12976331360947,8.30165680473373,8.47355029585799,8.64544378698225,8.81733727810651,8.98923076923077,9.16112426035503,9.33301775147929,9.50491124260355,9.67680473372781,9.84869822485207,10.0205917159763,10.1924852071006,10.3643786982249,10.5362721893491,10.7081656804734,10.8800591715976,11.0519526627219,11.2238461538462,11.3957396449704,11.5676331360947,11.7395266272189,11.9114201183432,12.0833136094675,12.2552071005917,12.427100591716,12.5989940828402,12.7708875739645,12.9427810650888,13.114674556213,13.2865680473373,13.4584615384615,13.6303550295858,13.8022485207101,13.9741420118343,14.1460355029586,14.3179289940828,14.4898224852071,14.6617159763314,14.8336094674556,15.0055029585799,15.1773964497041,15.3492899408284,15.5211834319527,15.6930769230769,15.8649704142012,16.0368639053254,16.2087573964497,16.380650887574,16.5525443786982,16.7244378698225,16.8963313609467,17.068224852071,17.2401183431953,17.4120118343195,17.5839053254438,17.755798816568,17.9276923076923,18.0995857988166,18.2714792899408,18.4433727810651,18.6152662721893,18.7871597633136,18.9590532544379,19.1309467455621,19.3028402366864,19.4747337278107,19.6466272189349,19.8185207100592,19.9904142011834,20.1623076923077,20.334201183432,20.5060946745562,20.6779881656805,20.8498816568047,21.021775147929,21.1936686390533,21.3655621301775,21.5374556213018,21.709349112426,21.8812426035503,22.0531360946746,22.2250295857988,22.3969230769231,22.5688165680473,22.7407100591716,22.9126035502959,23.0844970414201,23.2563905325444,23.4282840236686,23.6001775147929,23.7720710059172,23.9439644970414,24.1158579881657,24.2877514792899,24.4596449704142,24.6315384615385,24.8034319526627,24.975325443787,25.1472189349112,25.3191124260355,25.4910059171598,25.662899408284,25.8347928994083,26.0066863905325,26.1785798816568,26.3504733727811,26.5223668639053,26.6942603550296,26.8661538461538,27.0380473372781,27.2099408284024,27.3818343195266,27.5537278106509,27.7256213017751,27.8975147928994,28.0694082840237,28.2413017751479,28.4131952662722,28.5850887573964,28.7569822485207,28.928875739645,29.1007692307692,29.2726627218935,29.4445562130178,29.616449704142,29.7883431952663,29.9602366863905,30.1321301775148,30.3040236686391,30.4759171597633,30.6478106508876,30.8197041420118,30.9915976331361,31.1634911242604,31.3353846153846,31.5072781065089,31.6791715976331,31.8510650887574,32.0229585798817,32.1948520710059,32.3667455621302,32.5386390532544,32.7105325443787,32.882426035503,33.0543195266272,33.2262130177515,33.3981065088757,33.57),
            y = c (-7.98679996024651e-05,0.00121434910044422,0.00256443171801447,0.00402624537063184,0.00565565557581991,0.00751229848871183,0.00968000719585743,0.0122521735914465,0.0153222130544116,0.0189835006246645,0.02330432555373,0.0282851350782682,0.0339150206414871,0.0401830736865945,0.0470783856567985,0.0545792028600282,0.0625629316858093,0.0708521910019735,0.0792690540003063,0.0876355938725931,0.0957738838106195,0.103509100124729,0.110724168135627,0.117353076421299,0.123331573178646,0.128595406604566,0.133080324895958,0.136722076249719,0.139457224456307,0.14127125737049,0.142225488506753,0.142387753240976,0.141825886949037,0.140607725006816,0.138801102790193,0.136473855675047,0.133693818875213,0.130525807724143,0.127023895893784,0.123239769786541,0.119225115804821,0.115031620351031,0.110710969827576,0.106314850636865,0.101894949181302,0.0975029518632956,0.093189509544557,0.0889821899053127,0.0848858781414336,0.0809045040552263,0.0770419974489975,0.0733022881250539,0.0696893058857023,0.0662069805332494,0.0628592418700017,0.0596500196982662,0.0565832438203493,0.0536627159183011,0.0508881887940631,0.0482546389268256,0.045756767940725,0.0433892774598979,0.0411468691084808,0.0390242445106101,0.0370161052904224,0.0351171530720541,0.0333220894796419,0.0316256161373221,0.0300224346692313,0.0285072466995061,0.027074863312973,0.0257212404244086,0.0244430161319991,0.0232368376180389,0.0220993520648223,0.021027206654644,0.0200170485697982,0.0190655249925794,0.0181692831052821,0.0173249700902006,0.0165292331296294,0.0157787194058629,0.0150700761011955,0.0143999503979217,0.013764989480973,0.013162221711736,0.0125901138832118,0.0120474720351086,0.0115331022071344,0.0110458104389974,0.0105844027704057,0.0101476852410674,0.00973446389069063,0.00934354475898352,0.00897373388565415,0.00862383731041067,0.00829266107296116,0.00797901121301379,0.00768169377027663,0.00739951478445784,0.00713128029526549,0.00687579638048814,0.00663203356296345,0.00639950341299156,0.00617782861017872,0.00596663183413117,0.00576553576445519,0.00557416308075706,0.00539213646264302,0.00521907858971931,0.00505461214159221,0.00489835979786797,0.00474994423815287,0.00460898814205314,0.00447511418917506,0.00434794505912486,0.00422710343150883,0.00411221198593323,0.0040028934020043,0.00389877035932832,0.00379946553751152,0.00370460425536783,0.00361385833429135,0.00352693935723531,0.00344356018778255,0.00336343368951584,0.003286272726018,0.00321179016087185,0.00313969885766019,0.00306971167996583,0.00300154149137161,0.00293490115546028,0.00286950353581474,0.00280506149601771,0.00274128789965203,0.00267789561030052,0.00261459749154601,0.00255110640697127,0.00248713522015912,0.00242239679469237,0.00235660399415386,0.00228946968212637,0.0022207067221927,0.0021500279779357,0.00207720758887741,0.00200226763508291,0.00192529289913828,0.00184636816363311,0.00176557821115699,0.00168300782429951,0.00159874178565023,0.0015128648777988,0.00142546188333475,0.00133661758484768,0.00124641676492722,0.00115494420616292,0.00106228469114438,0.000968523002461188,0.000873743922702903,0.000778032234459168,0.000681472720319556,0.000584150162873616,0.000486149344710993,0.000387555048421238,0.000288452056593937,0.000188925151818713,8.90591166851261e-05,-1.10612662172144e-05,-0.000111351214298759,-0.000211725944969872)
    )
    ##  END  ##
