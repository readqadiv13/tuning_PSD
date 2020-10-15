    ##  (�L-`) .�oO (PSD simulator only available for the data of Microtrac MT3300EXII, 2018-12-11)

    source (file.path (poolDir, 'PSD_archive.R'), chdir = F)

    ##  Excel reader for sevral sheets  == (2019-11-01) ========================
    getExcel  <-  function (...) {
            Files  <<-  dir (pattern = 'xls|xlsx|���x����') %>%  {. [! str_detect (., '\\$|csv|pdf')]}  %>%  {if (length (.) > 1) {chooseOne. (., '\"�ړI��Excel�t�@�C��\"')} else {.}}
            ##  Reading excel
            fullPath  <-  file.path (getwd (), Files)
            sheN  <-  excel_sheets (path = fullPath)
            dtCurve  <-  read_excel (fullPath, sheet = str_which (sheN, '���z���z�`��'))  %>%  psdLab. (.)    #  For redesign, no matter what D50 is
            dtMaster  <-  read_excel (fullPath, sheet = str_which (sheN, '�}�X�^�['))  %>%  psdLab. (.)    #  The ace sample
            dtBase1  <-  read_excel (fullPath, sheet = str_which (sheN, '���1|���A'))  %>%  psdLab. (.)
            dtBase2  <-  read_excel (fullPath, sheet = str_which (sheN, '���2|���B'))  %>%  psdLab. (.)
            dtBase3  <-  read_excel (fullPath, sheet = str_which (sheN, '���3|���C'))  %>%  psdLab. (.)
            dtReal  <-  read_excel (fullPath, sheet = str_which (sheN, '����'))  %>%  psdLab. (.)
            ##  Warning shortage of the reading data
            if (! alive (dtCurve) && ! alive (dtMaster)) stop ('���z���z�`�󂨂�у}�X�^�[sheet�̃f�[�^�����s�����Ă��܂��D\n\n', call. = F)
            if (! alive (dtBase1))  stop ('��� A sheet�̃f�[�^�����s�����Ă��܂��D\n\n', call. = F)
            if (! alive (dtBase2))  stop ('��� B sheet�̃f�[�^�����s�����Ă��܂��D\n\n', call. = F)
            if (! alive (dtBase3))  dtBase3 [1, ]  <-  NA
            if (! alive (dtReal))  dtReal [1, ]  <-  NA
            ##  Select each one for 'Base' and 'Plus' data
            selectRow  <-  function (dt, tag = 'SELECTION ...', ... ) {
                    tenta  <-  namePull (dt)
                    { if (length (tenta) == 1)  dt  else chooseOne. (tenta, tag)  %>%  {which (tenta == .)}  %>%  dt [., ] }  %>%  return (.)
            }
            ##  Priority lays in dtMaster;  (dtCurve > 1, dtMaster > 1) -->  dtMaster wins and changes into 'dtRef'.
            if (alive (dtCurve) && ! alive (dtMaster)) {
                    dtRef  <-  selectRow (dtCurve, '\"�^�[�Q�b�g���z���ǂꂩ1�I��ŉ�����\"')
                    shift  <-  T
            } else if (alive (dtMaster)) {
                    dtRef  <-  selectRow (dtMaster, '\"�^�[�Q�b�g���z���ǂꂩ1�I��ŉ�����\"')
                    shift  <-  F
            }
            ##  Simulate or testify?
            tenta  <-  'tentative'
            if (alive (dtReal))  tenta  <-  chooseOne. (c ('�V�~�����[�V����', '����'), '�ǂ���̔ԍ������s���܂���')
            if (tenta == '�V�~�����[�V����')  dtReal  <-  dtReal [1, ] %>%  mutate_all (~ NA)    #  Prepare a dead tibble whether it's dead or alive
            if (tenta == '����') {    #  '����' only shows one graph.
                    if (nrow (dtReal) > 1)  dtReal  <- selectRow (dtReal, '\"���؃f�[�^���ǂꂩ1�I��ŉ�����\"')
                    if (nrow (dtBase1) > 1)  dtBase1  <- selectRow (dtBase1, '\"��� A �͂ǂ�ł��傤��\"')
                    if (nrow (dtBase2) > 1)  dtBase2  <- selectRow (dtBase2, '\"��� B �͂ǂ�ł��傤��\"')
                    if (nrow (dtBase3) > 1)  dtBase3  <- selectRow (dtBase3, '\"��� C �͂ǂ�ł��傤��\"')
            }
            list (ref = dtRef, b1 = dtBase1, b2 = dtBase2, b3 = dtBase3, real = dtReal, shift = shift, refD50 = dtRef $ 'D50') %>%  return (.)
    }

    ##  Check existance  == (2019-11-02) ========================
    alive  <-  function (dt, ... ) {
            if ('data.frame' %in% class (dt)) {
                    {rowSums (is.na (dt)) < ncol (dt) *0.9} %>%  sum. (.)  %>%  {. >= 1}  %>%  return (.)
            } else {    #  In case of vector;  it is possible when calling a list of iLcalc
                    unlist (dt)  %>%  {! is.na (.)}  %>% all (.)  %>%  return (.)
            }
    }

    ##  Get ID label  == (2019-05-01) ========================
    namePull  <-  function (dt, ... ) {
            if (is.na (dt) %>%  all (.))  return (NULL)
            if (map. (dt ['���x'], ~ skipMess. (ymd (.))) %>% {! anyNA (.)})  dt ['���x']  <-  map. (dt ['���x'], str_sub, 6, 10) #  2019/5/10 as chr
            if (map. (dt ['���x'], str_detect, pattern = '��|��') %>% any (.))  dt ['���x']  <-  map. (dt ['���x'], ~ gsub ('��', '-', .) %>% gsub ('��', '', .))    #  5��10��
            'tidyr' :: unite (dt [c ('�u����', '���x', '���b�g�ԍ�')], sep = ' :: ', col = ID)  %>%  pull (.)  %>%  gsub ('/', '|', .) %>%  return (.)
    }

    ##  Interactive input for the target D50 for dfRef to slide == (2019-07-24) ========================
    chooseD50  <-  function (df, microtracD50, use.D50, ... ) {    #  df denots your ideal curve; dfRef
            minP  <-  min. (df [, 1])
            quasiD50  <-  which.max (df [, 2])  %>%  df [., 1]    #  quasiD50 := peak in the probability function curve
            D50  <-  ifelse (use.D50, microtracD50, quasiD50)
            repeat {
                    num  <-  readline ('\n   What\'s your target D50?  \n\n   >>>  ')
                    if (zenk. (num)  %>%  {skipMess. (as.numeric (.))}  %>%  {! is.na (.)} ) {
                            num  <-  zenk. (num)  %>% as.numeric (num)
                            minShift  <-  minP -(D50 -num)
                            if (minShift < 0)  cat ('\n   Your input is too small to design PSD...\n\n')
                            if (num >= 0 && minShift >= 0)  break    #  This if () restricts minus D50
                    }
            }
            cat (paste0 ('\n', str_dup ('=', times = 75), '\n\n'))
            return (num -D50)
    }

    ##  Predictor  == (2019-11-09) ========================
    tunePSD  <-  function (...) {
            eng  <-  ifelse (Lang == '�p��', T, F)
            use.D50  <-  ifelse (D50 == '�}�C�N���g���b�N����l', T, F)
            ##  First preparation
            dexL  <-  getExcel ()    #  list data;  Ref = 1, Base1 >= 1, Base2 >= 1, Base3 >= 0, Real >= 0
            def. (c ('dt0', 'dt1', 'dt2', 'dt3', 'dt4'), list (dexL $ 'ref', dexL $ 'b1', dexL $ 'b2', dexL $ 'b3', dexL $ 'real' ))
            def. (c ('dfRef', 'dfReal'), list (getXYlines. (dt0, cook = T) [[1]], getXYlines. (dt4, cook = F) [[1]] ))
            if (dexL $ 'shift') {
                    dfRef [, 1]  <-  dfRef [, 1] +chooseD50 (dfRef, microtracD50 = dexL $ 'refD50', use.D50)
                #  dfRef [, 1]  <-  dfRef [, 1] -0.4    #  D50 = 9.9 --> 9.5 parallel shift (redesign for TZS sawmark, 60 um)
                #  dfRef [, 1]  <-  dfRef [, 1] -0.5    #  D50 = 9.5 --> 9.0 parallel shift (redesign for TZS sawmark, 48 um)
            }
            refx  <-  dfRef [, 1]
            ##  Making directory
            oldDir  <-  getwd ()
            newDir  <-  paste0 (oldDir, '/#Graphs')
            if (! file.exists (newDir))  dir.create ('#Graphs')
            setwd (newDir)
            ##  Create graph name
            fn  <-  str_split (Files, '\\.') [[1]] [1]  %>% gsub ('/|:|<|>|"|\\?|\\*|\\|', '_', .)  %>%  ifelse (Sys.getenv ('OS') == '', ., iconv (., 'utf-8', 'cp932'))  %>%
                      gsub ('���x����_', '', .)
            tod  <-  today (tz = 'Asia/Tokyo')  %>%  gsub ('-', '', .)  %>%  str_sub (., 3, 8)  %>%  str_c (., '_')
            time_stamp  <-  str_sub (fn, 1, 7)  %>% {skipMess. (parse_number (.))}  %>%  {if (is.na (.))  tod else  str_sub (fn, 1, 7)}
            tag  <-  function (dt)  if (! alive (dt))  NULL else  paste0 ('_', dt $ '���b�g�ԍ�')
            grN  <-  if (! alive (dfReal))  paste0 (time_stamp, 'simulate')  else  paste0 (time_stamp, 'verify', tag (dt1), tag (dt2), tag (dt3))
            save2. (grN, WH = c(5.5, 3.3))
            ##  Start it up
            rec  <-  rep (NA, 15)  %>%  t (.)  %>% {skipMess. (as_tibble (.))}  %>%  mutate_at (1, as_datetime, tz = 'Asia/Tokyo')  %>%    #  Vacant tibble for recording
                        setNames (c ('�v�Z��', map. (c ('�u����', '���x', '���b�g�ԍ�', '�z����'), ~ paste0 (., c ('A', 'B', 'C'))), 'D50�덷��', '�E�ȕ��ʐς̌덷��' ) )
            rec [1, 1]  <-  now ()
            for (i in 1: nrow (dt1)) for (j in 1: nrow (dt2)) for (k in 1: nrow (dt3)) {
                    ##  Selection
                    di  <-  bind_rows (slice (dt1, i), slice (dt2, j), slice (dt3, k))    #  Make a tibble of a combination with dt1 (i), dt2 (j), and dt3 (k)
                    rec [1, 2:10]  <-  unlist (di [c ('�u����', '���x', '���b�g�ԍ�')])
                    if (Sys.getenv ('OS') != '')  rec [1, 5:7] <-  map. (unlist (rec [1, 5:7]), ~ if (! is.na (.))  paste0 ('\'', .)  else  .)    #  Add ' to 6-12 so as to read it on excel...
                    ##  Calculation for the mixing ratio
                    iLcalc  <-  getXYlines. (di, cook = T) %>%  setNames (c ('b1', 'b2', 'b3'))
                    cx  <-  c (iLcalc $ 'b1' [, 1], iLcalc $ 'b2' [, 1], if (! alive (iLcalc $ 'b3'))  NULL  else  iLcalc $ 'b3'  [, 1])  %>%  unique (.)  %>%  sort (.)
                    def. (c ('yRef', 'yBase1', 'yBase2', 'yBase3'), list (miniYiv. (dfRef, refx), miniYiv. (iLcalc $ 'b1', cx), miniYiv. (iLcalc $ 'b2', cx), miniYiv. (iLcalc $ 'b3', cx)))
                    result_fit  <-  rssFit2. (yRef, yBase1, yBase2, yBase3, refx, cx)
                    Ans  <-  result_fit $ 'Ans'
                    rec [1, 11:13]  <-  ifelse (! alive (di [3, ]), NA, Ans [3])  %>%  c (Ans [-3], .)    #  To show vacant cell in the output csv when C isn't used
                    d50  <-  if (! alive (dfReal))  result_fit $ 'D50_match'  else  dt4 $ 'D50' /refx [which.max (yRef)] -1    # Simu := D50 of PDF, Veri := D50 of measured
                    rec [1, 14:15]  <-  c (d50, result_fit $ 'Tail_match')
                    ##  Drawing data
                    iLraw  <-  getXYlines. (di, cook = F) %>%  setNames (c ('b1', 'b2', 'b3'))
                    dfMix  <-  data.frame (x = cx, y = Ans [1] *yBase1 +Ans [2] *yBase2 +if (! alive (yBase3))  0  else  Ans [3] *yBase3)
                    dL  <-  list (dfRef, iLraw $ 'b1', iLraw $ 'b2', if (! alive (iLraw $ 'b3'))  NA  else  iLraw $ 'b3', dfMix, dfReal)
                    ##  Legend data
                    ni  <-  c (namePull (di [1, ]), namePull (di [2, ]), if (! alive (di [3, ]))  NULL  else  namePull (di [3, ]) )
                    text_pred  <-  ifelse (eng, 'Prediction', '���l�\��')  %>%  str_c (., ':  (A) ', sprintf ('%.2f', Ans [1] *100), '%, (B) ', sprintf ('%.2f', Ans [2] *100) )  %>%
                                         {if (! alive (di [3, ])) str_c (., '%')  else  str_c (., '%, (C) ', sprintf ('%.2f', Ans [3] *100), '%')}
                    text_D50  <-  ifelse (eng, 'D50 error rate: ', 'D50�덷��: ')  %>%  str_c (., sprintf ('%.2f', d50 *100), '%')
                    Texts  <-  c ('Target',
                                       str_c ('(A)  ', ni [1]),
                                       str_c ('(B)  ', ni [2]),
                                       if (! alive (di [3, ])) NULL  else  str_c ('(C) ', ni [3]),
                                       text_pred,
                                       if (! alive (dfReal)) NULL  else  ifelse (eng, 'Test result', '����'),
                                       text_D50)
                    ##  Drawing
                    pred_col  <-  c ('blue3', 'orange1', 'purple3', 'firebrick2', 'springgreen3')  %>%  rep (., times = nrow (dt1) *nrow (dt2) *nrow (dt3))
                    color  <-  c ('grey13', 'seashell4', 'snow4', if (! alive (dt3)) NULL else 'bisque4',
                                      pred_col [i], if (! alive (dfReal)) 'coral1' else c ('aquamarine3', 'chartreuse3'))
                    ltys  <-  c (1,2,3, if (! alive (dt3)) NULL else 4, 1, if (! alive (dfReal)) 0 else c (1, 0))
                    quickPlot. (dL, Ltys = ltys, xlab = 'Particle Size (��m)', col = color, Ylims = c (0, NA), name = Texts, PDF = F)
                    ##  Recording
                    recs  <-  if (i *j *k == 1)  rec  else bind_rows (recs, rec)
                    if (nrow(dt1) *nrow(dt2) *nrow(dt3) != 1) {
                        cts <- if (i == 1 && j == 1 && k == 1) 1 else cts +1
                        cat(str_c('    i = ', cts, ' (/', nrow(dt1) *nrow(dt2) *nrow(dt3), ')  finished:  ', now(), '\n'))
                    }
            }
            if (names (dev.cur ()) == 'cairo_pdf')  dev.off () #  For simulation
         #  if (alive (dfReal))  save. ('jpeg', name = grN)    # For verification
            write. (arrange (recs, abs (D50�덷��), abs (�E�ȕ��ʐς̌덷��)), name = grN)    #  Output table with sorting so as to choose better combinations easily
            setwd (oldDir)
            cat ('\n    ... Drawing completed.\n\n')
    }

    ##  RUN  ##
    tunePSD ()
    ##  END  ##