          Dcl-F CASE_DSPF Workstn Sfile(ADSBRD : RRN1) Sfile(CITYWIN : RRN2)
                                  Sfile(ADLTSFL : RRN3) Sfile(VHLDY : RRN4)
                                  Sfile(MEMPDTLS : RRN5) Sfile(MLEVSTS : RRN6)
                                  Sfile(MAPPROVE : RRN7) Sfile(MLSTS : RRN8)
                                  Sfile(MCSTS : RRN9) Sfile(MVHLD : RRN10);
          Dcl-F EMP_MASTER Usage(*Output : *Input : *Update : *Delete) Keyed;
          Dcl-S RRN1 Packed(4:0);
          Dcl-S RRN2 Packed(4:0);
          Dcl-S RRN3 Packed(4:0);
          Dcl-S RRN4 Packed(4:0);
          Dcl-S RRN5 Packed(4:0);
          Dcl-S RRN6 Packed(4:0);
          Dcl-S RRN7 Packed(4:0);
          Dcl-S RRN8 Packed(4:0);
          Dcl-S RRN9 Packed(4:0);
          Dcl-S RRN10 Packed(4:0);
          Dcl-S L_ID zoned(4);
          Dcl-S C_ID zoned(4);
          Dcl-S hdat zoned(8);
          Dcl-S hdat1 zoned(8);
          Dcl-S hdat2 zoned(8);
          Dcl-S H_DAY Char(10);
          dcl-s DATE1 date(*iso);
          dcl-s DATE2 date(*iso);
          dcl-s L_STS varchar(8);
          dcl-s C_STS varchar(8);
          dcl-s C_TYP varchar(4);
          dcl-s L_TTLDAY zoned(2);
          Dcl-S ROLE_CHK Char(10);
          dcl-s C_NAME varchar(20);
          dcl-s L_NAME varchar(20);
          Dcl-S ROLE_1 Varchar(20);
          Dcl-S CL_CNT Packed(3:1);
          Dcl-S EL_CNT Packed(3:1);
          Dcl-S DLT_ID Packed(4:1);
          dcl-s $DATE_A varchar(10);
          dcl-s $DATE_B varchar(10);
          Dcl-S CUR_DATE date(*usa);
          Dcl-S NET_CNT Packed(3:1);
          Dcl-S DAY_CNT Packed(3:1);
          Dcl-S LIV_CNT Packed(3:1);
          Dcl-S LIV_TYP1 Varchar(2);
          Dcl-S LIV_TYP2 Varchar(2);
          Dcl-S CTY_NAME Varchar(40);
          Dcl-S REMARK_1 Varchar(40);
          Dcl-S REASON_1 Varchar(40);
          Dcl-S CHK_ROLE Varchar(20);
          Dcl-S HDAY_CNT Packed(2:0);
          Dcl-S CLIV_CNT Packed(3:1);
          Dcl-S USER_ID1 Packed(4:1);
          Dcl-S LIV_CNT1 Packed(3:1);
          Dcl-S LIV_CNT2 Packed(3:1);
          Dcl-S LIV_CNT3 Packed(3:1);
          Dcl-S PRE_DATE1 date(*usa);
          Dcl-S I Packed(2:0) Inz(1);
          Dcl-S SLCT_TEAM Varchar(10);
          Dcl-S LIVBL_CNT Packed(3:1);
          Dcl-S GROSS_CNT Packed(3:1);
          Dcl-S MONTH_CNT Packed(2:0);
          Dcl-S VALID_CNT Packed(5:0);
          Dcl-S CHECK_DAY Packed(2:0);
          Dcl-S Arr Packed(5:0) Dim(5);
          Dcl-S TOTAL_DAY1 Packed(4:1);
          Dcl-S CHECK_DAY1 Packed(2:0);
          Dcl-S C_PASSWORD Varchar(20);
          Dcl-S INCREMENT_CNT Packed(2:0);

          Dcl-Ds ADM_DS Qualified;
             Z_ID    Packed(4:0) Inz;
             Z_NAME  Varchar(20) Inz;
             Z_CITY  Varchar(15) Inz;
             Z_ROLE  Varchar(20) Inz;
          End-Ds ADM_DS;


     ***************************** LOGIN PAGE *****************************
            Exec sql set option commit = *none,
                           closqlcsr = *endmod;

            Dow *In03 = *Off;
              Exfmt Loginr;

              Select;
                When *In03 = *On;
                  Leave;
                When *In05 = *On;
                  Exsr $Refresh_Sr;
                Other;
                  Exsr $Login_Sr;
              Endsl;
            Enddo;
          *Inlr = *On;



     **==> REFRESH SUBROUTINE ..........................

          Begsr $Refresh_Sr;
            Reset Uid;
            Reset Upass;
            *In05 = *Off;
            *In51 = *Off;
            *In52 = *Off;
            *in09 = *on;
            *in11 = *off;
          Endsr;

     **==> LOGIN VALIDATION ............................

          Begsr $Login_Sr;
            If Uid = *Zero;
              *In09 = *On;
              *In51 = *On;
              Leavesr;
            Endif;
            Exec sql
              Select user_id into :USER_ID1 from LOGIN_PF
              Where user_id = :uid ;
              If sqlcode <> 0;
                *In09 = *On;
                *In51 = *On;
                Leavesr;
              ElseIf cfld = 'UID';
                *in11 = *On;
                *in09 = *Off;
                Leavesr;
              Endif;

            Select;
              When Upass = *Blank;
                *In11 = *On;
                *In09 = *Off;
                *In52 = *On;
                *In51 = *Off;
                Leavesr;
              Other;
                Exec Sql
                  Select l.user_id,l.password,emp.role into :USER_ID1,
                    :C_PASSWORD, :ROLE_1 from LOGIN_PF l
                    Inner join EMP_MASTER emp
                    On emp.emp_id = l.user_id
                    Where user_id = :uid and
                          password = :upass;

                  If sqlcode = 0;

                    Select;
                      When ROLE_1 = 'ADMIN';
                        Exsr $admin_sr;

                      When ROLE_1 = 'MANAGER';
                        Exsr $addclel_sr;
                        Exsr $manager_sr;

                      When ROLE_1 = 'EMPLOYEE';
                        Exsr $addclel_sr;
                        Exsr $EMPLOYEE_SR;

                    Endsl;
                  Else;
                    *In09 = *On;
                    *In51 = *On;
                    Leavesr;
                  Endif;
                Reset Uid;
                Reset Upass;
            Endsl;
          Endsr;


     **==> CL/EL AUTO INCREMENT IN THE MONTH END............

          Begsr $Addclel_sr;
            ROLE_1 = *blank;
            Exec sql
              Select d.l_date,r.role into :hdat , :ROLE_1
                from LEAVEBL_PF d
                Inner join EMP_MASTER r
                On r.emp_id = d.emp_bid
                Where r.emp_id = :uid;

            Exec sql
              Select (current date) into :DATE1
                from SYSIBM.SYSDUMMY1;

                MONTH_CNT = 0;
                INCREMENT_CNT = 0;
                DATE2 = (%date(%char(%date(hdat : *usa):*iso)));
                MONTH_CNT = %diff(DATE2 : DATE1 : *months);
                If DATE1 <> DATE2;
                  Select;
                    When MONTH_CNT = 1 ;
                      INCREMENT_CNT = 1;
                    When MONTH_CNT = 2 ;
                      INCREMENT_CNT = 2;
                    When MONTH_CNT = 3 ;
                      INCREMENT_CNT = 3;
                    When MONTH_CNT = 4 ;
                      INCREMENT_CNT = 4;
                    When MONTH_CNT = 5 ;
                      INCREMENT_CNT = 5;
                    When MONTH_CNT = 6 ;
                      INCREMENT_CNT = 6;
                    When MONTH_CNT = 7 ;
                      INCREMENT_CNT = 7;
                    When MONTH_CNT = 8 ;
                      INCREMENT_CNT = 8;
                    When MONTH_CNT = 9 ;
                      INCREMENT_CNT = 9;
                    When MONTH_CNT = 10;
                      INCREMENT_CNT = 10;
                    When MONTH_CNT = 11;
                      INCREMENT_CNT = 11;
                    When MONTH_CNT = 12;
                      INCREMENT_CNT = 12;
                  Endsl;
                  hdat1 = %dec(%char(DATE1:*usa0):8:0);
                  Exec sql
                    update LEAVEBL_PF set
                      l_date = :hdat1 ,
                      CL = CL + :INCREMENT_CNT * .5 ,
                      EL = EL + :INCREMENT_CNT * 1
                    where emp_bid = :uid;
                Endif;
          Endsr;


     **  ****************STARTING OF ADMIN DASHBOARD***************
     **  ************************************************************

          Begsr $ADMIN_SR;
            Dow *in03 = *off;
              Exsr $clear_sr;
              Exsr $load_sr;
              Write footer;
              Scrnvar = '  ADMIN DASHBOARD';
              Exsr $display_sr;

              If *in12 = *on;
                EXSR $Refresh_sr;
                Leavesr;

              Elseif *in05 = *on;
                EXSR $refresh_sr;
                Reset aad_pos;

              Elseif *in06 = *on;
                Exsr $ADD_EMPLOYEE_SR;

              Elseif *in07 = *on;
                Exsr $ADD_HOLIDAY_SR;

              Elseif *in08 = *on;
                Exsr $MANAGER_LIV_SR;

              Elseif *in10 = *on;
                Exec sql
                  Select COUNT(*) Into :HDAY_CNT
                    From HOLIDAY_PF;
                    If HDAY_CNT < 1;
                      SCS_WIN = '  Add Holidays To See';
                      EXFMT SUCCESSWIN;
                      HDAY_CNT = 0;
                    Else;
                      EXSR $AVHLD_SR;
                      HDAY_CNT = 0;
                    Endif;
              Else;
                Readc ADSBRD;
                i = 1;
                *IN02 = *ON;
                DOW NOT %EOF();
                  If aadopt = 2;
                    Exsr $UPDATE_SR;

                  Elseif aadopt = 4;
                    Arr(i) = aad_id;
                    i += 1;

                  Elseif aadopt = 5;
                    Exsr $ADISPLY_SR;
                  Endif;
                  Readc adsbrd;
                Enddo;

                If aadopt = 4;
                  Exsr $DELETE_SR;
                Endif;
                *In02 = *Off;
                Reset aadopt;
              Endif;
            Enddo;
            Reset aad_pos;
          Endsr;

     **==> ADMIN DASHBOARD CLEAR,LOAD,DISPLAY...........

          Begsr $clear_sr;
            *In91 = *off;
            Write adsbrdct;
            *In91 = *on;
            Rrn1 = 0;
          Endsr;

          Begsr $load_sr;
            Exec sql declare c1 cursor for
              Select z.emp_id,z.emp_name,z.city,z.role
                From EMP_MASTER z
              Where emp_id >= :aad_pos and
                z.role <> 'ADMIN' order by emp_id ;

              Exec sql open c1;
              Exec sql fetch from c1 into :ADM_DS;
              if sqlcode = 0;
                *in71 = *off;
                DoW sqlcode = 0;
                  Rrn1 = rrn1 + 1;
                  Aad_id = ADM_DS.Z_ID;
                  Aad_name = ADM_DS.Z_NAME ;
                  Aad_city = ADM_DS.Z_CITY ;
                  Aad_role = ADM_DS.Z_ROLE;
                  Aad_user = 'ADMIN';
                  Write adsbrd;
                  Exec sql fetch from c1 into :ADM_DS;
                Enddo;
              else;
                *in71 = *On;
              endif;
              Exec sql close c1;
          Endsr;

          Begsr $display_sr;
            Exfmt adsbrdct;
          Endsr;

     ** ==> F6 = ADD EMPLOYEE .............................

          Begsr $ADD_EMPLOYEE_SR;
            Reset add_emp;
            *in13 = *on;
            *in80 = *on;
            Dow *in03 = *off;
              Scrnvar = 'ADD EMPLOYEE';
              Addcntry = 'INDIA';
              Exec sql select max(emp_id) into :USER_ID1
                From EMP_MASTER;

              Addid = USER_ID1 + 1;
              *In01 = *Off;
              Usern = 'ADMIN';
              Exfmt ADD_EMP;

              Select;
                When *in12 = *on;
                  Leavesr;

                When *in05 = *on;
                  Reset add_emp;
                  Scrnvar = 'ADD EMPLOYEE';
                  Addcntry = 'INDIA';

                When cfld = 'ADDGEN' and *in04 = *on;
                  Exsr $gen_sr;
                  Exsr $check_add_empinpt_sr;

                When cfld = 'ADDCITY' AND *in04 = *on;
                  If *in12 = *on;
                    Leavesr;
                  Else;
                    Exsr $ctyclear;
                    Exsr $ctyload;
                    Exsr $ctydsp;
                    Exsr $check_add_empinpt_sr;
                  Endif;

                When cfld = 'ADDTEAM' AND *in04 = *on;
                  Exsr $team_sr;
                  Exsr $check_add_empinpt_sr;

                When cfld = 'ADDSTS' AND *in04 = *on;
                  Exsr $status_sr;
                  Exsr $check_add_empinpt_sr;

                When cfld = 'ADDROLE' AND *in04 = *on;
                  Exsr $role_sr;
                  Exsr $check_add_empinpt_sr;

                Other;
                  Exsr $check_add_empinpt_sr;

                  If *in53 = *off and *in54 = *off and *in55 = *off
                    and *in56 = *off and *in57 = *off and *in58 = *off
                    and *in59 = *off and *in60 = *off and *in61 = *off
                    and *in62 = *off and *in63 = *off and *in64 = *off
                    and *in65 = *off and *in66 = *off and *in67 = *off
                    and *in68 = *off and *in69 = *off and *in70 = *off
                    and *in72 = *off and *in73 = *off and *in74 = *off
                    and *in31 = *on ;


     **==> WRITE DATA IN EMP_MASTER...........................
                    Exec sql
                    Insert into EMP_MASTER
                    ( EMP_ID,EMP_NAME,ADDRESS,PIN_CODE,CITY,COUNTRY,
                      CONTACT_NO,EMER_NO,GENDER,DOB,DOJ,EMAIL_P,EMAIL_O,
                      TEAM,PROJECT_ID,MANAGER_ID,ROLE,DESIG,STATUS )
                    VALUES
                    ( :ADDID  , left(:ADDNAME , 30) ,  left(:ADDADR , 50),
                      :ADDPIN , :ADDCITY , :ADDCNTRY , :ADDMOB ,
                      :ADDAMOB , :ADDGEN , :ADDDOB , :ADDDOJ  ,
                      left(:ADDEMLP , 30) ,  left(:ADDEMLO , 30) ,
                      :ADDTEAM , :ADDPID  , :ADDMID , :ADDROLE ,
                      :ADDDESIG  , :ADDSTS );

                    If Sqlcode = 0;
                      Exec sql
                      Insert into LOGIN_PF
                      ( USER_ID, PASSWORD )
                      VALUES
                      ( :ADDID , '12345' );

                      Exec sql
                      Insert into LEAVEBL_PF
                      ( EMP_BID, L_DATE )
                      VALUES
                      ( :ADDID , :ADDDOJ );
                      SCS_WIN = '   SUCCESSFULLY SUBMITTED';
                      Reset add_emp;
                      EXFMT SUCCESSWIN;

                    Endif;
                  Endif;
              Endsl;
            Enddo;
          Endsr;


     **==> CHECK_INPUT_VALIDATION..................

          Begsr $CHECK_ADD_EMPINPT_SR;

     **   Check_For_Name.......................

            If ADDNAME = *blank;
              Exsr $clear_indicater_sr;
              *In53 = *on;
              *In13 = *on;
              Leavesr;
            Endif;

            If %check('abcdefghijklmnopqrstuvwxyz ': %trim(ADDNAME)) <> 0;
              Exsr $clear_indicater_sr;
              *In53 = *on;
              *In13 = *on;
              Leavesr;
            Endif;

            If CFLD = 'ADDNAME';
              Exsr $clear_indicater_sr;
              *in14 = *on;
              Leavesr;
            Endif;

     **   Check_For_Gender......................

            If ADDGEN = *blank;
              Exsr $clear_indicater_sr;
              *In54 = *on;
              *In14 = *on;
              Leavesr;
            Endif;

            If CFLD = 'ADDGEN';
              Exsr $clear_indicater_sr;
              *in15 = *on;
              Leavesr;
            Endif;

     **   Check_For_City..........................

            If ADDCITY = *blank;
              Exsr $clear_indicater_sr;
              *In55 = *on;
              *In15 = *on;
              Leavesr;
            Endif;

            If CFLD = 'ADDCITY';
              Exsr $clear_indicater_sr;
              *in16 = *on;
              Leavesr;
            Endif;

     **   Check_For_Dob.............................

            If ADDDOB = *zero;
              Exsr $clear_indicater_sr;
              *In56 = *on;
              *In16 = *on;
              Leavesr;
            Endif;

            Test(de) *usa ADDDOB ;
            If %error;
              Exsr $clear_indicater_sr;
              *in56 = *on;
              *in16 = *on;
              Leavesr;
            Endif;

            If CFLD = 'ADDDOB';
              Exsr $clear_indicater_sr;
              *in17 = *on;
              Leavesr;
            Endif;

     **  Check For Doj................................
            If ADDDOJ = *zero;
              Exsr $clear_indicater_sr;
              *In57 = *on;
              *In17 = *on;
              Leavesr;
            Endif;

            Test(de) *usa ADDDOJ ;
            If %error;
              Exsr $clear_indicater_sr;
              *in57 = *on;
              *in17 = *on;
              Leavesr;
            Endif;

     **   Check_For_18+.........................
            DATE1 = (%date(%char(%date(ADDDOJ:*usa):*iso)));
            DATE2 = (%date(%char(%date(ADDDOB:*usa):*iso)));

            If %diff(DATE1 : DATE2 : *years
               ) < 18;
              Exsr $clear_indicater_sr;
              *In72 = *on;
              *In17 = *on;
              Leavesr;
            Endif;
     **   Check_For_not greater than current date......
            Exec sql
              Select (current date) into :CUR_DATE
                From SYSIBM.SYSDUMMY1;
                If date1 > CUR_DATE ;
                  *In72 = *on;
                  *In17 = *on;
                  Leavesr;
                Endif;

                If CFLD = 'ADDDOJ';
                  Exsr $clear_indicater_sr;
                  *in18 = *on;
                  Leavesr;
                Endif;

     **   Check_for_Address..............................

                If ADDADR = *blank;
                  Exsr $clear_indicater_sr;
                  *In58 = *on;
                  *In18 = *on;
                  Leavesr;
                Endif;

                If CFLD = 'ADDADR';
                  Exsr $clear_indicater_sr;
                  *in20 = *on;
                  Leavesr;
                Endif;



     **   Check_for_pincode.....................

                If ADDPIN = *zero;
                  Exsr $clear_indicater_sr;
                  *In60 = *on;
                  *In20 = *on;
                  Leavesr;
                Endif;

                If CFLD = 'ADDPIN';
                  Exsr $clear_indicater_sr;
                  *in21 = *on;
                  Leavesr;
                Endif;



     **   Check_For_Team.....................

                If ADDTEAM = *blank;
                  Exsr $clear_indicater_sr;
                  *In61 = *on;
                  *In21 = *on;
                  Leavesr;
                Endif;

                Exec sql
                  Select TEAM into :SLCT_TEAM
                    From EMP_MASTER
                  Where TEAM = :Addteam and ROLE = 'MANAGER' and
                    emp_id <> :Addid and STATUS = 'ACTIVE';
                  If Addrole = 'MANAGER' and scrnvar = 'UPDATE MANAGER' and
                    sqlcode = 0 and Addsts = 'ACTIVE';
                    Exsr $clear_indicater_sr;
                    *In61 = *on;
                    *In21 = *on;
                    leavesr;
                  Endif;

                If CFLD = 'ADDTEAM';
                  Exsr $clear_indicater_sr;
                  *in22 = *on;
                  Leavesr;
                Endif;

     **   Check_For_Project_Id................

                If ADDPID = *zero;
                  Exsr $clear_indicater_sr;
                  *In62 = *on;
                  *In22 = *on;
                  Leavesr;
                Endif;

                If CFLD = 'ADDPID';
                  Exsr $clear_indicater_sr;
                  *in23 = *on;
                  Leavesr;
                Endif;

     **   Check_For_Designation.........................

                If ADDDESIG = *blank;
                  Exsr $clear_indicater_sr;
                  *In63 = *on;
                  *In23 = *on;
                  Leavesr;
                Endif;

                If %len(%trim(ADDDESIG)) < 5;
*                 Exsr $clear_indicater_sr;
                  *In63 = *on;
                  *In23 = *on;
                  Leavesr;
                Endif;
                If %check('abcdefghijklmnopqrstuvwxyz':
                          %TRIM(ADDDESIG)) <> 0;
                  Exsr $clear_indicater_sr;
                  *In63 = *on;
                  *In23 = *on;
                  Leavesr;
                Endif;

                If CFLD = 'ADDDESIG';
                  Exsr $clear_indicater_sr;
                  Exec sql
                    Select MANAGER_ID into :Addmid from EMP_MASTER
                    where TEAM = :Addteam and ROLE = 'MANAGER';
                    *in24 = *on;
                    Leavesr;
                Endif;

     **   Check_For_Manager_Id..................



                If ADDMID = *zero;
*                 Exsr $clear_indicater_sr;
                  *In64 = *on;
                  *In24 = *on;
                  Leavesr;
                Endif;

                If CFLD = 'ADDMID';
                  Exsr $clear_indicater_sr;
                  *in25 = *on;
                  Leavesr;
                Endif;

     **   Check_For_Mobile_No..................

                If ADDMOB = *zero;
                  Exsr $clear_indicater_sr;
                  *In65 = *on;
                  *In25 = *on;
                  Leavesr;
                Endif;

                If CFLD = 'ADDMOB';
                  Exsr $clear_indicater_sr;
                  *in26 = *on;
                  Leavesr;
                Endif;

     **   Check_For_Alternate_Mobile_No............

                If ADDAMOB = *zero;
                  Exsr $clear_indicater_sr;
                  *In66 = *on;
                  *In26 = *on;
                  Leavesr;
                Endif;

     **   Check_For_Duplicate_Mobile_Number.....................

                If ADDAMOB = ADDMOB;
                  Exsr $clear_indicater_sr;
                  *in73 = *on;
                  *in26 = *on;
                  leavesr;
                Endif;

                If CFLD = 'ADDAMOB';
                  Exsr $clear_indicater_sr;
                  *in27 = *on;
                  Leavesr;
                Endif;

     **   Check_For_Valid_Email_Office..................

                If ADDEMLO = *blank;
                  Exsr $clear_indicater_sr;
                  *In67 = *on;
                  *In27 = *on;
                  Leavesr;
                Endif;

                Exec sql
                  Set :VALID_CNT = RegExp_count( trim(:ADDEMLO),
                            '^[\w-\.]+§([\w-]+\.)+[\w-]{2,4}$');

                If VALID_CNT <> 1;
                  Exsr $clear_indicater_sr;
                  *In67 = *on;
                  *In27 = *on;
                  Leavesr;
                Endif;

                If CFLD = 'ADDEMLO';
                  Exsr $clear_indicater_sr;
                  *in28 = *on;
                  Leavesr;
                Endif;

     **   Check_For_Valid_Email_Personal..................

                If ADDEMLP = *blank;
                  Exsr $clear_indicater_sr;
                  *In68 = *on;
                  *In28 = *on;
                  Leavesr;
                Endif;

                Exec sql
                  Set :VALID_CNT = RegExp_count( rtrim(:ADDEMLP),
                             '^[\w-\.]+§([\w-]+\.)+[\w-]{2,4}$');

                If VALID_CNT <> 1;
                  Exsr $clear_indicater_sr;
                  *In68 = *on;
                  *In28 = *on;
                  Leavesr;
                Endif;

     **   Check_For_Same_Email......................

                If ADDEMLP = ADDEMLO;
                  Exsr $clear_indicater_sr;
                  *In74 = *on;
                  *In28 = *on;
                  Leavesr;
                Endif;

                If CFLD = 'ADDEMLP';
                  Exsr $clear_indicater_sr;
                  *in29 = *on;
                  Leavesr;
                Endif;

     **   Check_For_Status.....................

                If ADDSTS = *blank;
                  Exsr $clear_indicater_sr;
                  *In69 = *on;
                  *In29 = *on;
                  Leavesr;
                Endif;

                If CFLD = 'ADDSTS';
                  Exsr $clear_indicater_sr;
                  *in30 = *on;
                  Leavesr;
                Endif;

     **   Check_For_Role.....................

                If ADDROLE = *blank;
                  Exsr $clear_indicater_sr;
                  *In70 = *on;
                  *In30 = *on;
                  Leavesr;
                Endif;

                Exec sql
                  Select TEAM,EMP_NAME into :SLCT_TEAM,:C_NAME
                    From EMP_MASTER
                  Where TEAM = :ADDTEAM and ROLE = 'MANAGER'
                    and STATUS = 'ACTIVE';
                  If ADDROLE = 'MANAGER' and SCRNVAR = 'UPDATE MANAGER'
                    and ADDSTS = 'ACTIVE' and sqlcode = 0 and
                    ADDNAME <> C_NAME;
                    ADDROLE = ROLE_CHK;
                    Exsr $clear_indicater_sr;
                    *In75 = *on;
                    *In30 = *on;
                    leavesr;
                  Elseif sqlcode = 0 and SCRNVAR = 'UPDATE EMPLOYEE'
                    and ADDROLE = 'MANAGER' and ADDSTS = 'ACTIVE' and
                    ADDNAME <> C_NAME;
                    ADDROLE = ROLE_CHK;
                    Exsr $clear_indicater_sr;
                    *In75 = *on;
                    leavesr;
                  Elseif ADDROLE = 'MANAGER' and SCRNVAR = 'ADD EMPLOYEE'
                    and sqlcode = 0;
                    Exsr $clear_indicater_sr;
                    *In75 = *on;
                    *In30 = *on;
                    leavesr;
                  Endif;

                Exec sql
                  Select TEAM into :SLCT_TEAM
                    From EMP_MASTER
                  Where TEAM = :ADDTEAM and ROLE = 'MANAGER'
                    and STATUS = 'INACTIVE';

                    If ADDSTS = 'ACTIVE' and ADDROLE = 'MANAGER' and
                      SCRNVAR = 'ADD EMPLOYEE' and sqlcode <> 0 ;
                      Exec sql
                        Select MANAGER_ID into :Addmid from EMP_MASTER
                        where TEAM = :Addteam and ROLE = 'MANAGER';
                      *in80 = *on ;
                    Else;
                      *in80 = *off;
                    Endif;

                If CFLD = 'ADDROLE';
                  Exsr $clear_indicater_sr;
                  *in31 = *on;
                  *in71 = *on;
                  Leavesr;
                Endif;
          Endsr;

     **    ************ VALIDATION CHECK ENDED ****************
     **    ****************************************************
     **    *************  PROMPT SELECTION  *******************

     **  ==> F4 = GENDER_SELECTION..................

          Begsr $gen_sr;
            *In04 = *off;
            Exfmt genwin;
            If Gen = 1;
              ADDGEN = 'MALE';
            Elseif gen = 2;
              ADDGEN = 'FEMALE';
            Elseif gen = 3;
              ADDGEN = 'OTHER';
            Endif;
          Endsr;


     **  ==> F4 = CITY_SELECTION....................

     **  Clear_sr.................

          Begsr $CTYCLEAR;
            *In90 = *off;
            Write citywinc;
            *In90 = *on;
            Rrn2 = 0;
          Endsr;

     **  Load_sr..................

          Begsr $CTYLOAD;
            Exec sql declare c2 cursor for
              Select * from citypf;
                Exec sql open c2;
                Exec sql fetch from c2 into :CTY_NAME;

                DoW sqlcode = 0;
                  Rrn2 = rrn2 + 1;
                  Ctynam = CTY_NAME;
                  Write citywin;
                  Exec sql fetch from c2 into :CTY_NAME;
                Enddo;
                Exec sql close c2;
          Endsr;

     **  Display_sr...............

          Begsr $CTYDSP;
            Exfmt citywinc;
            Readc citywin;
            DOW NOT %EOF();
              *In02 = *on;
              If Ctyopt = 1;
                Addcity = ctynam;
              Endif;
              Readc citywin;
            Enddo;
            *In02 = *off;
            Reset ctyopt;
          Endsr;


     **  ==> F4 = TEAM SELECTION....................

          Begsr $TEAM_SR;
            *in04 = *off;
            Exfmt TIMWIN;
            If Tim = 1;
              Addteam = 'RPG TEAM';
            Elseif tim = 2;
              Addteam = 'COBOL TEAM';
            Elseif tim = 3;
              Addteam = 'JAVA TEAM';
            Elseif tim = 4;
              Addteam = 'ANGULAR TEAM';
            Endif;
          Endsr;

     **  ==> F4 = STATUS SELECTION..............

          Begsr $STATUS_SR;
            *In04 = *off;
            Exfmt STSWIN;
            If Sts = 1;
              Addsts = 'ACTIVE';
            Elseif STS = 2;
              Addsts = 'INACTIVE';
            Endif;
          Endsr;

     **  ==> F4 = ROLE SELECTION.............

          Begsr $ROLE_SR;
            *in04 = *off;
            Exfmt ROLWIN;
            If ROL = 1;
              Addrole = 'MANAGER';
            Elseif ROL = 2;
              Addrole = 'EMPLOYEE';
            Endif;
          Endsr;

     **  ************ PROMPT SELECTION ENDED ****************

     **  CLEAR INDICATERS SUBROUTINE.......

          Begsr $clear_indicater_sr;
            *In53 = *off;
            *In54 = *off;
            *In55 = *off;
            *In56 = *off;
            *In57 = *off;
            *In58 = *off;
            *In59 = *off;
            *In60 = *off;
            *In61 = *off;
            *In62 = *off;
            *In63 = *off;
            *In64 = *off;
            *In65 = *off;
            *In66 = *off;
            *In67 = *off;
            *In68 = *off;
            *In69 = *off;
            *In70 = *off;
            *In71 = *off;
            *In72 = *off;
            *In73 = *off;
            *In74 = *off;
            *In75 = *off;
            *In76 = *off;
            *In77 = *off;
            *In78 = *off;
            *In79 = *off;
            *In80 = *off;
            *In13 = *off;
            *In14 = *off;
            *In15 = *off;
            *In16 = *off;
            *In17 = *off;
            *In18 = *off;
            *In19 = *off;
            *In20 = *off;
            *In21 = *off;
            *In22 = *off;
            *In23 = *off;
            *In24 = *off;
            *In25 = *off;
            *In26 = *off;
            *In27 = *off;
            *In28 = *off;
            *In29 = *off;
            *In30 = *off;
            *In31 = *off;
          Endsr;

     ** *************** ADMIN_DASHBOARD_OPTIONS *****************

     **  ==> 2 = UPDATE_DATA.................

          Begsr $UPDATE_SR;
            Reset add_emp;
            *in80 = *on;
            Exsr $lodupdta_sr;
            Dow *in03 = *off;
              *in49 = *on;
              If ROLE_CHK = 'EMPLOYEE';
                Scrnvar = 'UPDATE EMPLOYEE';
              Elseif ROLE_CHK = 'MANAGER';
                Scrnvar = 'UPDATE MANAGER';
              Endif;
              Usern = 'ADMIN';
              Exfmt add_emp;

              Select;
                When *In12 = *on;
                  Reset AADOPT;
                  Leave;

                When *In05 = *on;
                  Reset ADD_EMP;
                  Exsr $lodupdta_sr;

                when cfld = 'ADDCITY' and *in04 = *on;
                  If *In12 = *on;
                    Leavesr;
                  Else;
                    Exsr $ctyclear;
                    Exsr $ctyload;
                    Exsr $ctydsp;
                    Exsr $check_add_empinpt_sr;
                  Endif;

                When cfld = 'ADDTEAM' AND *in04 = *on;
                  Exsr $team_sr;
                  Exsr $check_add_empinpt_sr;

                when cfld = 'ADDSTS' AND *in04 = *on;
                  Exsr $status_sr;
                  Exsr $check_add_empinpt_sr;

                When cfld = 'ADDROLE' AND *in04 = *on;
                  If Addrole = 'EMPLOYEE';
                    Exsr $role_sr;
                    Exsr $check_add_empinpt_sr;
                  Elseif Addrole = 'MANAGER';
                    SCS_WIN = 'UNABLE TO CHANGE MANAGER ROLE';
                    exfmt SUCCESSWIN ;
                  Endif;
                Other;
                  EXSR $CHECK_ADD_EMPINPT_SR;
                  EXSR $UPDTASNTX_SR;
              Endsl;
            Enddo;
            *in49 = *off;
          Endsr;


     **  ==> Loding_Data_For_Update...............

          Begsr $Updtasntx_sr;
            Select;
              When *in53 = *off and *in54 = *off and *in55 = *off and
                *in56 = *off and *in57 = *off and *in58 = *off and
                *in59 = *off and *in60 = *off and *in61 = *off and
                *in62 = *off and *in63 = *off and *in64 = *off and
                *in65 = *off and *in66 = *off and *in67 = *off and
                *in68 = *off and *in69 = *off and *in70 = *off and
                *in72 = *off and *in73 = *off and *in74 = *off and
                *in75 = *off and *in71 = *on and *in31 = *on and
                Addcity = CITY and  Addadr = ADDRESS and
                Addcntry = COUNTRY and  Addpin = PIN_CODE and
                Adddoj = DOJ and Addteam = TEAM and
                Addpid = PROJECT_ID and Addmid = MANAGER_ID and
                Addmob = CONTACT_NO and Addamob = EMER_NO and
                Addemlp = EMAIL_P and Addemlo = EMAIL_O and
                Adddesig = DESIG and Addsts = STATUS and
                Addrole = ROLE;

                  SCS_WIN = '      NO CHANGE FOUND';
                  exfmt SUCCESSWIN ;
                  Exsr $clear_indicater_sr;


              When *in53 = *off and *in54 = *off and *in55 = *off and
                *in56 = *off and *in57 = *off and *in58 = *off and
                *in59 = *off and *in60 = *off and *in61 = *off and
                *in62 = *off and *in63 = *off and *in64 = *off and
                *in65 = *off and *in66 = *off and *in67 = *off and
                *in68 = *off and *in69 = *off and *in70 = *off and
                *in71 = *on and *in72 = *off and *in73 = *off and
                *in74 = *off and *in75 = *off and *in31 = *on ;

                  Exec sql
                    Update EMP_MASTER set
                    ADDRESS = left(:ADDADR , 50) ,  PIN_CODE = :ADDPIN ,
                    CITY = :ADDCITY ,  DOJ = :ADDDOJ ,
                    COUNTRY = :ADDCNTRY ,  CONTACT_NO = :ADDMOB ,
                    EMER_NO = :ADDAMOB ,  EMAIL_P = left(:ADDEMLP , 30) ,
                    EMAIL_O = left(:ADDEMLO , 30) ,  TEAM = :ADDTEAM ,
                    PROJECT_ID = :ADDPID  ,  MANAGER_ID = :ADDMID ,
                    ROLE = :ADDROLE ,  DESIG = :ADDDESIG  ,
                    STATUS = :ADDSTS
                    Where EMP_ID = :ADDID;

                    If Sqlcode = 0 ;
                      SCS_WIN = '    SUCCESSFULLY UPDATED';
                      exfmt SUCCESSWIN ;
                    Endif;
            Endsl;
          Endsr;


     **  ==> Data_Load_Subroutine_For_Update .....................

          Begsr $lodupdta_sr;
            EMP_ID = AAD_ID;
            chain (EMP_ID) EMP_MASTER;
            ADDID =EMP_ID;
            ADDNAME = EMP_NAME;
            ADDGEN = GENDER;
            ADDCITY = CITY;
            ADDDOB = DOB ;
            ADDDOJ = DOJ ;
            ADDADR = ADDRESS;
            ADDCNTRY = COUNTRY;
            ADDPIN = PIN_CODE;
            ADDTEAM = TEAM;
            ADDPID = PROJECT_ID;
            ADDMID = MANAGER_ID;
            ADDMOB = CONTACT_NO;
            ADDAMOB = EMER_NO;
            ADDEMLP = EMAIL_P;
            ADDEMLO = EMAIL_O;
            ADDDESIG = DESIG;
            ADDSTS = STATUS;
            ADDROLE = ROLE;
            ROLE_CHK = ROLE;
          Endsr;


     **  ==> 5 = DISPLAY_DATA..........

          Begsr $ADISPLY_SR;
            Reset ADD_EMP;
            Exsr $lodupdta_sr;
            Usern = 'ADMIN';
            If ROLE_CHK = 'EMPLOYEE';
                Scrnvar = 'EMPLOYEE DETAILS';
              Elseif ROLE_CHK = 'MANAGER';
                Scrnvar = 'MANAGER DETAILS';
              Endif;
            *in01 = *on;
            *in50 = *on;
            *in80 = *on;
            *in49 = *on;
            Exfmt ADD_EMP;

            Select;
              When *in03 = *on;
                Exsr $endpgm_sr;
              When *in12 = *on;
                Reset AADOPT;
                *in01 = *off;
                *in50 = *off;
                *in80 = *off;
                *in49 = *off;
                Leavesr;
            Endsl;
          Endsr;


     **  ==> 4 = DELETE_DATA...........

          Begsr $DELETE_SR;
            Exsr $dclear_sr;
            Exsr $dload_sr;
            Usern = 'ADMIN';
            Exsr $ddisplay_sr;

            Select;
              When *in12 = *on;
                Reset AADOPT;
                Reset arr;
                Leavesr;

              When *in03 = *on;
                Exsr $endpgm_sr;

              Other;
                For i = 1 to %elem(arr);
                  DLT_ID = Arr(i);
                  Exec sql
                    Delete from EMP_MASTER where emp_id = :DLT_ID;
                  Exec sql
                    Delete from LOGIN_PF where USER_ID = :DLT_ID;
                  Exec sql
                    Delete from COMP_PF where EMP_CID = :DLT_ID;
                  Exec sql
                    Delete from LEAVEBL_PF where EMP_BID = :DLT_ID;
                  Exec sql
                    Delete from LEAVE_PF where EMP_LID = :DLT_ID;
                Endfor;
                SCS_WIN = '    SUCCESSFULLY DELETED';
                  exfmt SUCCESSWIN ;
            Endsl;
          Endsr;

     **  Delete_clear_sr.....

          Begsr $dclear_sr;
            *In92 = *off;
            Write adltsflc;
            *In92 = *on;
            Rrn3 = 0;
          Endsr;

     **  Delete_load_sr......

          Begsr $dload_sr;
            For i = 1 to %elem(arr);
              DLT_ID = arr(i);
              Exec sql
                Select z.emp_id,z.emp_name,z.city,z.role Into :ADM_DS
                  From EMP_MASTER z
                  Where emp_id = :DLT_ID;
                  If Sqlcode = 0;
                    Rrn3 = rrn3 + 1;
                    Dltid = ADM_DS.Z_ID;
                    Dltname = ADM_DS.Z_NAME ;
                    Dltcity = ADM_DS.Z_CITY ;
                    Dltrole = ADM_DS.Z_ROLE;
                    Write adltsfl;
                  Endif;
            Endfor;
          Endsr;

     **  Delete_Display_Sr........

          Begsr $ddisplay_sr;
            Write dltfooter;
            Exfmt adltsflc;
          Endsr;


     **  ==> F3 = END_PROGRAMME.......

          Begsr $endpgm_sr;
            *Inlr = *on;
          Endsr;



     **  ==> F7 = ADD_HOLIDAYS...........

          Begsr $ADD_HOLIDAY_SR;
            *in31 = *off;
            Reset ADHLD;
            Dow *in03 = *off;
              Ahscrn = 'ADD HOLIDAY';
              Exfmt ADHLD;
              Select;
                When *in12 = *on;
                  Leave;
                When *in05 = *on;
                  Reset ADHLD;
                Other;
                  Exsr $addates_sr;
                  if *in53 = *off and *in54 = *off and
                    *in55 = *off and *in31 = *on and
                    *in71 = *on ;
                    Exec sql
                      Insert into HOLIDAY_PF
                        (DATE,DAY,REMARKS)
                      VALUES
                        ( :AAHDATE ,
                        :AAHDAY ,
                        :AAHRMRK );
                      If sqlcode = 0 ;
                        SCS_WIN = '  HOLIDAY ADDED SUCCESSFULLY';
                        Reset ADHLD;
                        EXFMT SUCCESSWIN;
                        *in31 = *off;
                        *in71 = *off;
                      Endif;
                  Endif;
              Endsl;
            Enddo;
          Endsr;


     **  ==> Date_Check_Sr..........

          Begsr $addates_sr;
            If AAHDATE = *zero;
              Exsr $clear_indicater_sr;
              *In53 = *on;
              *In13 = *on;
              Leavesr;
            Endif;

            Test(de) *usa AAHDATE ;
            If %error;
              Exsr $clear_indicater_sr;
              *in53 = *on;
              *in13 = *on;
              Leavesr;
            Endif;

            Exec sql
              Select DATE into :hdat from HOLIDAY_PF
                Where DATE = :AAHDATE;
                If Hdat = AAHDATE;
                  Exsr $clear_indicater_sr;
                  *In72 = *on;
                  *In13 = *on;
                  Leavesr;
                Endif;
            $DATE_A = %char(%date(AAHDATE:*USA):*ISO);


            Exec sql
              SELECT CASE DAYOFWEEK(:$DATE_A)
                When 1
                  Then 'Sunday'
                When 2
                  Then 'Monday'
                When 3
                  Then 'Tuesday'
                When 4
                  Then 'Wednesday'
                When 5
                  Then 'Thursday'
                When 6
                  Then 'Friday'
                Else
                  'Saturday'
                END CASE  INTO :AAHDAY
                FROM SYSIBM.SYSDUMMY1;

            If AFLD = 'AAHDATE';
              Exsr $clear_indicater_sr;
              *in14 = *on;
              Leavesr;
            Endif;

            if AAHDAY = *blank;
              Exsr $clear_indicater_sr;
              *In54 = *on;
              *In14 = *on;
              Leavesr;
            endif;


            If AFLD = 'AAHDAY';
              Exsr $clear_indicater_sr;
              *in15 = *on;
              Leavesr;
            Endif;

            If AAHRMRK = *blank;
              Exsr $clear_indicater_sr;
              *In55 = *on;
              *In15 = *on;
              Leavesr;
            Endif;

            If AFLD = 'AAHRMRK';
              Exsr $clear_indicater_sr;
              *in31 = *on;
              Leavesr;
            Endif;

            If *in31 = *on and *in71 = *off;
              Exsr $clear_indicater_sr;
              *in31 = *on;
              *in71 = *on;
              Leavesr;
            Endif;

          Endsr;


     **===> F8 = MANAGERs LEAVES APPROVE/DENY......

          Begsr $MANAGER_LIV_SR;
            Dow *in03 = *off;
              Alivusr = 'ADMIN';
              exfmt MLIVAPR;

              If *in12 = *on;
                Reset Alivslct;
                Leavesr;

              Elseif *in05 = *on;
                Reset Alivslct;

              Elseif alivslct = 1;
                Exec sql
                  Select COUNT(*) INTO :LIV_CNT
                    From LEAVE_PF l
                    inner join EMP_MASTER e
                    on e.emp_id = l.emp_lid
                  Where l.status = 'P' and  e.role = 'MANAGER';

                If LIV_CNT <  1;
                  SCS_WIN = '    NOTHING FOR UPDATE';
                  Reset Alivslct;
                  EXFMT SUCCESSWIN ;
                  LIV_CNT = 0;
                Else;
                  Exsr $MDAPRDNYL_SR;
                  LIV_CNT = 0;
                Endif;

              Elseif alivslct = 2;
                Exec sql
                  Select COUNT(*) INTO :CLIV_CNT
                    From COMP_PF c
                    inner join EMP_MASTER e
                    on e.emp_id = c.emp_cid
                  Where c.status = 'P' and e.role = 'MANAGER';

                  If CLIV_CNT < 1;
                    SCS_WIN = '    NOTHING FOR UPDATE';
                    Reset Alivslct;
                    EXFMT SUCCESSWIN ;
                    CLIV_CNT = 0;
                  Else;
                    Exsr $MDAPRDNYCMP_SR;
                    CLIV_CNT = 0;
                  Endif;
                Endif;
            Enddo;
          Endsr;


     **  ==> F10 = VIEW_HOLIDAYS............

          Begsr $avhld_sr;
            Dow *in03 = *off;
              Exsr $avhclear_sr;
              Exsr $avhload_sr;
              Exsr $avhdisplay_sr;

              If *in12 = *on;
                Reset avhopt;
                Leavesr;
              Endif;

              Readc VHLDY;
              Dow not %eof();
                *IN02 = *ON;
                If avhopt = 2;
                  Exsr $auhld_sr;
                Elseif avhopt = 4;
                  Exsr $adlthld_sr;
                Endif;
                  READC VHLDY;
              Enddo;
                *In02 = *off;
                Reset avhopt;
            Enddo;
          Endsr;

     **  View_Holiday_Clear....

          Begsr $avhclear_sr;
            *In93 = *off;
            Write VHLDYCNT;
            *In93 = *on;
            Rrn4 = 0;
          Endsr;

     **  View_Holiday_Load.....

          Begsr $avhload_sr;
            Exec sql declare c3 cursor for
              Select date, day, remarks from HOLIDAY_PF;
              Exec sql open c3;
              Exec sql fetch from c3 into :HDAT, :H_DAY, :REMARK_1;

              Dow sqlcode = 0;
                Rrn4 = rrn4 + 1;
                Avhdate = HDAT;
                Avhday = H_DAY ;
                Avhrmrk = REMARK_1 ;
                Write VHLDY;
                Exec sql fetch from c3 into :HDAT, :H_DAY, :REMARK_1;
              Enddo;
              Exec sql close c3;
          Endsr;

     **  View_Holiday_Display......

          Begsr $avhdisplay_sr;
            Write dltfooter;
            Reset AVHOPT;
            Exfmt VHLDYCNT;
          Endsr;

     **  *************** ADMIN HOLIDAY OPTIONS *******************

     **  ==> 2 = UPDATE............

          Begsr $auhld_sr;
            AAHDATE = AVHDATE ;
            AAHDAY = AVHDAY;
            AAHRMRK = AVHRMRK;
            Dow *in03 = *off;
              AHSCRN = 'UPDATE HOLIDAY';
              Exfmt ADHLD;
              *In51 = *off;
              *In53 = *off;

              Select;
                When *in12 = *on;
                  Exsr $clear_indicater_sr;
                  reset avhopt;
                  Leave;

                When *in05 = *on;
                  Exsr $clear_indicater_sr;
                  AAHDATE = AVHDATE ;
                  AAHDAY = AVHDAY;
                  AAHRMRK = AVHRMRK;

                Other;
                  Exsr $addates_sr;
                  if *in53 = *off and *in54 = *off and
                    *in55 = *off and *in31 = *on and
                    *in71 = *on ;
                    Exec sql
                      Update HOLIDAY_PF set
                        DATE = :AAHDATE ,
                        DAY = :AAHDAY ,
                        REMARKS = :AAHRMRK
                      Where DATE = :AVHDATE and REMARKS = :AVHRMRK;
                    If sqlcode = 0 ;
                      SCS_WIN = '   SUCCESSFULLY UPDATED';
                      EXFMT SUCCESSWIN;
                      *in31 = *off;
                      *in71 = *off;
                    Endif;
                    Exec sql
                      select date into :AVHDATE from HOLIDAY_PF
                      where day = :AAHDAY and REMARKS = :AVHRMRK;
                    leavesr;
                  Endif;
              Endsl;
            Enddo;
          Endsr;




     **  ==> 4 = DELETE............

          Begsr $adlthld_sr;
            Exec sql
              Delete from HOLIDAY_PF where DATE = :AVHDATE;
            SCS_WIN = '   SUCCESSFULLY DELETED';
            EXFMT SUCCESSWIN;
            Reset avhopt;
          Endsr;





     ** **************** END OF ADMIN DASHBOARD *****************
     ** *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
     ** ************ STARTING OF MANAGER DASHBOARD **************





          BEGSR $MANAGER_SR;
            Dow *in03 = *off;
              Exec sql select team into :mteam from EMP_MASTER
              Where emp_id = :uid;
              Reset mselect;
              Exfmt MGDSBRD;

              If *in12 = *on;
                Exsr $refresh_sr;
                Leave;

              Elseif *in05 = *on;
                Reset mselect;

     **  ==> 1 = Employee_details........

              Elseif mselect = 1;
                Exsr $MDEMPDTLS_SR;

     **  ==> 2 = Leave_Approval.....

              Elseif mselect = 2;
                LIV_CNT = 0;
                Exec sql
                  Select COUNT(*) INTO :LIV_CNT
                    From LEAVE_PF l
                    inner join EMP_MASTER e
                    on e.emp_id = l.emp_lid
                  Where l.status = 'P' and e.team = :mteam and
                    e.role = 'EMPLOYEE';
                  If LIV_CNT <  1;
                    SCS_WIN = '    NOTHING FOR UPDATE';
                    EXFMT SUCCESSWIN ;
                    LIV_CNT = 0;
                  Else;
                    Exsr $MDAPRDNYL_SR;
                    LIV_CNT = 0;
                  Endif;

     **  ==> 3 = Comp_Off_Approval......

              Elseif mselect = 3;
                CLIV_CNT = 0;
                Exec sql
                  Select COUNT(*) INTO :CLIV_CNT
                    From COMP_PF c
                    inner join EMP_MASTER e
                    on e.emp_id = c.emp_cid
                  Where c.status = 'P' and e.team = :mteam and
                    e.role = 'EMPLOYEE' and c.emp_cid <> :uid ;

                  If CLIV_CNT < 1;
                    SCS_WIN = '    NOTHING FOR UPDATE';
                    EXFMT SUCCESSWIN ;
                    CLIV_CNT = 0;
                  Else;
                    Exsr $MDAPRDNYCMP_SR;
                    CLIV_CNT = 0;
                  Endif;

     **  ==> 4 = Apply_Comp_Off......

              Elseif mselect = 4;
                Exec sql
                  Select COMP_LEAVE INTO :LIVBL_CNT
                    From LEAVEBL_PF
                  Where emp_bid = :uid;

                  If LIVBL_CNT < 1;
                    SCS_WIN = '    NOT ENOUGH LEAVE';
                    EXFMT SUCCESSWIN ;
                    LIVBL_CNT = 0;
                  Else;
                    Exsr $MDAPLYCMP_SR;
                    LIVBL_CNT = 0;
                  Endif;


     **  ==> 5 = Apply_Leaves........

              Elseif mselect = 5;
                Exsr $MDAPLYLIV_SR;


     **  ==> 6 = Leave_Status........

              Elseif mselect = 6;
                LIV_CNT = 0;
                Exec sql
                  Select COUNT(*) INTO :LIV_CNT
                    From LEAVE_PF
                  Where emp_lid = :uid;

                  If LIV_CNT < 1;
                    SCS_WIN = '   NOT APPLIED TILL NOW';
                    EXFMT SUCCESSWIN ;
                    LIV_CNT = 0;
                  Else;
                    Exsr $MDLIVSTS_SR;
                    LIV_CNT = 0;
                  Endif;


     **  ==> 7 = Comp_Off_Status........

              Elseif mselect = 7;
                CLIV_CNT = 0;
                Exec sql
                  Select COUNT(*) INTO :CLIV_CNT
                    From COMP_PF
                  Where emp_cid = :uid;

                  If CLIV_CNT < 1;
                    SCS_WIN = '   NOT APPLIED TILL NOW';
                    EXFMT SUCCESSWIN ;
                    CLIV_CNT = 0;
                  Else;
                    Exsr $MDCMPOSTS_SR;
                    CLIV_CNT = 0;
                  Endif;


     **  ==> 8 = View_Holiday...........

              Elseif mselect = 8;
                HDAY_CNT = 0;
                Exec sql
                  Select COUNT(*) INTO :HDAY_CNT
                    From HOLIDAY_PF;
                  If HDAY_CNT < 1;
                    SCS_WIN = '     NOTHING TO SHOW';
                    EXFMT SUCCESSWIN ;
                    HDAY_CNT = 0;
                  Else;
                    Exsr $MDVIWHLD_SR;
                    HDAY_CNT = 0;
                  Endif;


     **  ==> 9 = Change_Password........

              Elseif mselect = 9;
                Exsr $MDCHGPASS_SR;
              Elseif mselect = 10;
                Leavesr;
              Endif;
            Enddo;
          ENDSR;

     **  ************** MANAGER DASHBOARD SELECTION ***************
     **  *********************  SUBROUTINES  **********************

     **  ==> 1 = MANAGER EMPLOYEE DETAILS.....................

          Begsr $MDEMPDTLS_SR;
           dow *in03 = *Off;
            Exec sql
              select team into :teamm from EMP_MASTER
              Where emp_id = :uid;
              Exsr $mempdclear_sr;
              Exsr $mempdload_sr;
              Exsr $mempddsp_sr;

              Select;
                When *in12 = *on;
                  Reset mselect;
                  Leavesr;

                When *in03 = *on;
                  Exsr $endpgm_sr;

                Other;
                  Readc mempdtls;
                  Dow not %eof();
                    *In02 = *on;
                    If Medopt = 5;
                      Exsr $mempdtlsv_sr;
                    Endif;
                    Readc mempdtls;
                  Enddo;
                  *In02 = *off;
              Endsl;
           enddo;
          Endsr;

     **  ==> Emp_Details_Clear..........

          Begsr $mempdclear_sr;
            *In94 = *off;
            Write MEMPCNTR;
            *In94 = *on;
            Rrn5 = 0;
          Endsr;

     **  ==> Emp_Details_Load............
          Begsr $mempdload_sr;
            Exec sql declare c4 cursor for
              Select z.emp_id,z.emp_name,z.team,z.desig
                From EMP_MASTER z
              Where z.TEAM = :teamm ;
              Exec sql open c4;
              Exec sql fetch from c4 into :ADM_DS;

              DoW sqlcode = 0;
                Rrn5 = rrn5 + 1;
                MEDID = ADM_DS.Z_ID;
                MEDNAME = ADM_DS.Z_NAME ;
                MEDTEAM = ADM_DS.Z_CITY ;
                MEDDESIG = ADM_DS.Z_ROLE;
                Write MEMPDTLS;
                Exec sql fetch from c4 into :ADM_DS;
              Enddo;

              Exec sql close c4;
          Endsr;

     **  ==> Emp_Details_Display...........

          Begsr $mempddsp_sr;
            Write dltfooter;
            Exfmt MEMPCNTR;
          Endsr;


     **  ==> 1 = Employee_Details_View......

          Begsr $mempdtlsv_sr;
            Reset add_emp;
            *in49 = *on;
            *in50 = *on;
            *in80 = *on;
            Exsr $Mlodupdta_sr;
            *in01 = *on;
            Scrnvar = 'VIEW EMPLOYEE';
            Usern = 'MANAGER';
            Exfmt add_emp;

            If *in12 = *on;
              *in49 = *off;
              *in50 = *off;
              *in80 = *off;
              Reset MEDOPT;
              Leavesr;
            Endif;
            Reset medopt;
          Endsr;

     **  ==> Empl_Details_Load_For_View..........

          Begsr $Mlodupdta_sr;
            EMP_ID = MEDID;
            chain (emp_id) EMP_MASTER;
            ADDID =EMP_ID;
            ADDNAME = EMP_NAME;
            ADDGEN = GENDER;
            ADDCITY = CITY;
            ADDDOB = DOB;
            ADDDOJ = DOJ;
            ADDADR = ADDRESS;
            ADDCNTRY = COUNTRY;
            ADDPIN = PIN_CODE;
            ADDTEAM = TEAM;
            ADDPID = PROJECT_ID;
            ADDMID = MANAGER_ID;
            ADDMOB = CONTACT_NO;
            ADDAMOB = EMER_NO;
            ADDEMLP = EMAIL_P;
            ADDEMLO = EMAIL_O;
            ADDDESIG = DESIG;
            ADDSTS = STATUS;
            ADDROLE = ROLE;
          Endsr;


     **  ==> 2 = APPROVE/DENY_LEAVES...........

          Begsr $MDAPRDNYL_SR;
            *in06 = *off;
            Dow *in03 = *off;
              Exec sql
                Select role into :CHK_ROLE from EMP_MASTER
                Where emp_id = :uid;
                If sqlcode = 0 and CHK_ROLE = 'MANAGER';
                  MADUSR = 'MANAGER';
                  Exec sql select team into :MADTEAM from EMP_MASTER
                  Where emp_id = :uid ;
                  Exsr $madlvclear_sr;
                  Exsr $madlvload_sr;
                  Exsr $madlvdsp_sr;
                Else;
                  *In06 = *on;
                  MADUSR = 'ADMIN';
                  Exsr $madlvclear_sr;
                  Exsr $Aadlvload_sr;
                  Exsr $madlvdsp_sr;
                Endif;

              Select;
                When *in12 = *on;
                  Reset mselect;
                  Leavesr;

                When *in03 = *on;
                  Exsr $endpgm_sr;

                Other;
                  Readc MLEVSTS;
                  Dow not %eof();
                    *In02 = *on;
                    Exec sql
                      Select LEAVE_TYPE, TOTAL_DAY
                        Into :LIV_TYP2, :TOTAL_DAY1
                        From LEAVE_PF
                      Where emp_lid = :madid and FROM_DATE = :MADFDATE
                        and status = 'P';

                      If Madopt = 1 ;
                        Exec sql
                          Update LEAVE_PF set
                            STATUS = 'A'
                          Where emp_lid = :madid and
                            From_date = :Madfdate;

                      Elseif Madopt = 2 ;
                        Exec sql
                        Update LEAVE_PF set
                          STATUS = 'D'
                        Where emp_lid = :madid and
                          From_date = :madfdate;

                        Exec sql
                          Update LEAVEBL_PF set
                            CL = CASE :LIV_TYP2 WHEN 'CL'
                            THEN CL + :TOTAL_DAY1
                            ELSE CL END ,
                            EL = CASE :LIV_TYP2 WHEN 'EL'
                            THEN EL + :TOTAL_DAY1
                            ELSE EL END
                          Where emp_bid = :MADID;

                      Elseif madopt = 5;
                        Exsr $maddesc_sr;
                      Endif;
                      Readc MLEVSTS;
                  Enddo;
                  *In02 = *off;
              Endsl;
            Enddo;
          Endsr;

     **  A/D_Leaves_Clear.....

          Begsr $madlvclear_sr;
            *In95 = *off;
            Write MLEVSTSC;
            *In95 = *on;
            Rrn6 = 0;
          Endsr;

     **  A/D_Leaves_Load.............

          Begsr $madlvload_sr;
            Exec sql declare c5 cursor for
              Select z.EMP_LID,z.FROM_DATE,z.TO_DATE,
                z.TOTAL_DAY,z.STATUS,e.EMP_NAME
                From LEAVE_PF z
                Inner join EMP_MASTER e
                On e.emp_id = z.emp_lid
              Where z.status = 'P' and e.TEAM = :MADTEAM and
                e.role = 'EMPLOYEE';
            Exec sql open c5;
            Exec sql fetch from c5 into :L_ID,:HDAT,:HDAT1,
                                  :L_TTLDAY,:L_STS,:L_NAME;

              DoW sqlcode = 0;
                rrn6 = rrn6 + 1;
                MADID = L_ID;
                MADNAME = L_NAME ;
                MADFDATE = HDAT;
                MADTDATE = HDAT1;
                MADTTLDAY = L_TTLDAY;
                MADSTS = L_STS;
                Write MLEVSTS;
                Exec sql fetch from c5 into :L_ID,:HDAT,:HDAT1,
                                      :L_TTLDAY,:L_STS,:L_NAME;
              Enddo;
            Exec sql close c5;
          Endsr;

          Begsr $Aadlvload_sr;
            Exec sql declare c10 cursor for
              Select z.EMP_LID,z.FROM_DATE,z.TO_DATE,
                z.TOTAL_DAY,z.STATUS,e.EMP_NAME
                From LEAVE_PF z
                Inner join EMP_MASTER e
                On e.emp_id = z.emp_lid
              Where z.status = 'P' and e.ROLE = 'MANAGER'
                and z.emp_lid <> :uid;
            Exec sql open c10;
            Exec sql fetch from c10 into :L_ID,:hdat,:hdat1,
                                   :L_TTLDAY,:L_STS,:L_NAME;
              DoW sqlcode = 0;
                rrn6 = rrn6 + 1;
                MADID = L_ID;
                MADNAME = L_NAME ;
                MADFDATE = hdat;
                MADTDATE = hdat1;
                MADTTLDAY = L_TTLDAY;
                MADSTS = L_STS;
                Write MLEVSTS;
                Exec sql fetch from c10 into :L_ID,:hdat,:hdat1,
                                       :L_TTLDAY,:L_STS,:L_NAME;
              Enddo;
            Exec sql close c10;
          Endsr;


     **  A/D_Leaves_Display........

          Begsr $madlvdsp_sr;
            Write dltfooter;
            Exfmt MLEVSTSC;
          Endsr;


     **  ==> 2 = APPROVE/DENY_LEAVES_DESCRIPTION......

          Begsr $maddesc_sr;
            Dow *in03 = *off;
              Exsr $loaddisc_sr;
              Exec sql select team into :MLDTEAM from EMP_MASTER
                Where emp_id = :uid ;
              Exfmt MLEVDESC;

              if *in05 = *on;
                Reset mldselect;

              Elseif *in12 = *on;
                Reset mldselect;
                Reset mselect;
                Leavesr;

              Elseif MLDSELECT = 1 and MLDSTS = 'P';
                MLDSTS = 'A';
                Exec sql
                Update LEAVE_PF set
                STATUS = 'A'
                Where emp_lid = :MLDID;

              Elseif MLDSELECT = 2 and MLDSTS = 'P';
                MLDSTS = 'D';
                Exec sql
                  Update LEAVE_PF set
                    STATUS = 'D'
                  Where emp_lid = :mldid;

                Exec sql
                  Update LEAVEBL_PF set
                    CL = CASE :MLDLTYP WHEN 'CL'
                    THEN CL + :MLDTTLDAY
                    ELSE CL END ,
                    EL = CASE :MLDLTYP WHEN 'EL'
                    THEN EL + :MLDTTLDAY
                    ELSE EL END
                  Where emp_bid = :MLDID;
              Endif;
              Reset mldselect;
            Enddo;
          Endsr;

     **  A/D_Leave_Discription_load..........

          Begsr $loaddisc_sr;
            Exec sql
              Select z.EMP_LID,z.FROM_DATE,z.FROM_DAY,z.TO_DATE,
                z.TO_DAY,z.TOTAL_DAY,z.REASON,z.STATUS,z.LEAVE_TYPE,
                e.EMP_NAME,(x.CL + x.EL + x.COMP_LEAVE) AS TTL
              Into :MLDID,:MLDFDATE,:MLDFDAY,:MLDTDATE,:MLDTDAY,
                :MLDTTLDAY,:MLDRSN,:MLDSTS,:MLDLTYP,:MLDNAME,
                :MLDLBAL
              From LEAVE_PF z
              Inner join EMP_MASTER e
              On e.emp_id = z.emp_lid
              Inner join LEAVEBL_PF x
              On x.emp_bid = z.emp_lid
              Where z.emp_lid = :MADID and
                z.From_date = :MADFDATE and z.Status = 'P' ;

          Endsr;

     **  ==> 3(A&B) = APPROVE/DENY_COMP_OFF.........

          Begsr $MDAPRDNYCMP_SR;
            *in06 = *off;
            Dow *in03 = *off;
              Exec sql
                Select role into :CHK_ROLE from EMP_MASTER
                Where emp_id = :uid;
                If sqlcode = 0 and CHK_ROLE = 'MANAGER';
                  MADUSR = 'MANAGER';
                  Exec sql select team into :MADCTEAM from EMP_MASTER
                  Where emp_id = :uid;
                  Exsr $MADCCLEAR_SR;
                  Exsr $MADCLOAD_SR;
                  Exsr $MADCDSP_SR;
                Else;
                  *In06 = *on;
                  MADUSR = 'ADMIN';
                  Exsr $MADCCLEAR_SR;
                  Exsr $AADCLOAD_SR;
                  Exsr $MADCDSP_SR;
                Endif;


              Select;
                When *in12 = *on;
                  Reset mselect;
                  Leavesr;

                When *in03 = *on;
                  Exsr $endpgm_sr;

                Other;
                  Readc MAPPROVE;
                  Dow not %eof();
                    *In02 = *on;
                    If Madcopt = 1 ;
                      Exec sql
                        Update COMP_PF set
                          STATUS = 'A'
                        Where emp_cid = :MADCID and
                          Work_date = :MADCWDATE;

                    Elseif Madcopt = 2 ;
                      Exec sql
                        Update COMP_PF set
                          STATUS = 'D'
                        Where emp_cid = :madcid and
                          Work_date = :MADCWDATE;

                      Exec sql
                        Update LEAVEBL_PF set
                          COMP_LEAVE = COMP_LEAVE + 1
                        Where emp_bid = :MADCID;

                    Elseif MADCOPT = 5;
                      Exsr $madcdesc_sr;

                    Endif;
                    Readc MAPPROVE;
                  Enddo;
                  *In02 = *off;
              Endsl;
            Enddo;
          Endsr;

     **  A/D_Comp_Off_Desc_clear......

          Begsr $MADCCLEAR_SR;
            *In96 = *off;
            Write MAPPROVEC;
            *In96 = *on;
            Rrn7 = 0;
          Endsr;

     **  A/D_Comp_Off_Desc_Load.......

          Begsr $MADCLOAD_SR;
            Exec sql declare c6 cursor for
              Select z.EMP_CID,z.WORK_DATE,z.DAY_TYPE,
                z.STATUS,e.EMP_NAME
              From COMP_PF z
              Inner join EMP_MASTER e
              On e.emp_id = z.emp_cid
              Where z.status = 'P' and e.TEAM = :MADCTEAM and
                emp_cid <> :uid ;
            Exec sql open c6;
            Exec sql fetch from c6 into :C_ID,:HDAT,:C_TYP,
                                            :C_STS,:C_NAME;
              DoW sqlcode = 0;
                rrn7 = rrn7 + 1;
                MADCID = C_ID;
                MADCNAME = C_NAME ;
                MADCWDATE = HDAT;
                MADCDTYP = C_TYP;
                MADCSTS = C_STS;
                Write MAPPROVE;
                Exec sql fetch from c6 into :C_ID,:HDAT,:C_TYP,
                                                :C_STS,:C_NAME;
              Enddo;
            Exec sql close c6;
          Endsr;


          Begsr $AADCLOAD_SR;
            Exec sql declare c11 cursor for
              Select z.EMP_CID,z.WORK_DATE,z.DAY_TYPE,
                z.STATUS,e.EMP_NAME
              From COMP_PF z
              Inner join EMP_MASTER e
              On e.emp_id = z.emp_cid
              Where z.status = 'P' and e.ROLE = 'MANAGER';

            Exec sql open c11;
            Exec sql fetch from c11 into :C_ID,:HDAT,:C_TYP,
                                             :C_STS,:C_NAME;
              DoW sqlcode = 0;
                rrn7 = rrn7 + 1;
                MADCID = C_ID;
                MADCNAME = C_NAME ;
                MADCWDATE = HDAT;
                MADCDTYP = C_TYP;
                MADCSTS = C_STS;
                Write MAPPROVE;
                Exec sql fetch from c11 into :C_ID,:HDAT,:C_TYP,
                                                 :C_STS,:C_NAME;
              Enddo;
            Exec sql close c11;
          Endsr;


     **  A/D_Comp_Off_Desc_Display.......

          Begsr $MADCDSP_SR;
            Write dltfooter;
            Exfmt MAPPROVEC;
          Endsr;


     **  ==> 3C = APPROVE/DENY COMP_OFF DESCRIPTION......

          Begsr $madcdesc_sr;
            Dow *in03 = *off;
              Exec sql
                Select team into :MADCTEAM from EMP_MASTER
                Where emp_id = :uid;
              Exsr $loaddcmp_sr;
              Exfmt MCOMPDES;

              If *in05 = *on;
                Reset mcdselect;

              Elseif *in12 = *on;
                Reset mselect;
                Leavesr;

              Elseif MCDSELECT = 1 and MCDSTS = 'P';
                MCDSTS = 'A';
                Exec sql
                Update COMP_PF set
                STATUS = 'A'
                Where emp_cid = :madcid;

              Elseif MCDSELECT = 2 and MCDSTS = 'P';
                MCDSTS = 'D';
                Exec sql
                Update COMP_PF set
                STATUS = 'D'
                Where emp_cid = :madcid;

                Exec sql
                  Update LEAVEBL_PF set
                    COMP_LEAVE = COMP_LEAVE + 1
                  Where emp_bid = :MCDID;
              Endif;
              Reset mcdselect;
            Enddo;
          Endsr;

     **  Manager_Comp_Off_Desc_Load............

          Begsr $loaddcmp_sr;
            Exec sql
              Select z.EMP_CID,z.WORK_DATE,z.DAY_TYPE,z.REASON,
                z.STATUS,z.PROJECT_ID,e.EMP_NAME
              Into :MCDID,:MCDWDATE,:MCDDTYP,:MCDRSN,:MCDSTS,
                :MCDPID,:MCDNAME
              From COMP_PF z
              Inner join EMP_MASTER e
              On e.emp_id = z.emp_cid
              Where z.emp_cid = :MADCID and z.STATUS = 'P' and
                z.WORK_DATE = :MADCWDATE;
          Endsr;


     **  (4) MANAGER_APPLY_COMP_OFF ..............

          Begsr $MDAPLYCMP_SR;
            Reset MACOMPOF;
            Dow *in03 = *off;
              Exec sql
                Select PROJECT_ID,TEAM into :MACPID,:MACTEAM
                From EMP_MASTER
                Where emp_id = :uid;
                If ROLE_1 = 'MANAGER';
                  MACUSR = 'MANAGER';
                Else;
                  MACUSR = 'EMPLOYEE';
                Endif;
              Exfmt MACOMPOF;

              If *in12 = *on;
                Reset mselect;
                Leavesr;

              Elseif *in05 = *on;
                Reset MACOMPOF;

              Else;
                Exsr $chckkwdate_sr;

                If *in53 = *off and *in54 = *off and
                  *in55 = *off and *in56 = *off and
                  *in72 = *off and *in73 = *off and
                  *in74 = *off and *in71 = *on and
                  *in31 = *on ;
                  Exec sql
                  Insert into COMP_PF
                    ( EMP_CID,WORK_DATE,DAY_TYPE,REASON,STATUS,PROJECT_ID)
                  VALUES
                    ( :UID , :MACWDATE ,  'FD' , :MACRSN , 'P' ,  :MACPID );

                  If sqlcode = 0;
                    Exec Sql
                      Update LEAVEBL_PF set
                        COMP_LEAVE  = COMP_LEAVE - 1
                      Where emp_bid = :uid;
                    SCS_WIN = '    SUCCESSFULLY APPLIED';
                    Reset MACOMPOF;
                    EXFMT SUCCESSWIN ;
                  Endif;
                Endif;
              Endif;
            Enddo;
          Endsr;


     **    ********** APPLY_COMP_OFF_ALL_VALIDATION **********

          Begsr $chckkwdate_sr;

     **  ==> Date_Check.......
            If MACWDATE = *Zero;
              Exsr $clear_indicater_sr;
              *In53 = *on;
              *In13 = *on;
              Leavesr;
            Endif;

            Test(de) *usa MACWDATE ;
            If %error;
              Exsr $clear_indicater_sr;
              *in53 = *on;
              *in13 = *on;
              Leavesr;
            Endif;

            Exec sql
              Select work_date into :HDAT
              From COMP_PF
              Where work_date = :MACWDATE and emp_cid = :uid
                   and Status in ('A','P');
              If MACWDATE = HDAT;
                Exsr $clear_indicater_sr;
                *in72 = *on;
                *in13 = *on;
                Leavesr;
              Endif;


              Exec sql
                Select DATE into :HDAT1 from HOLIDAY_PF
                Where DATE = :MACWDATE;

              $DATE_A = %char(%date(MACWDATE:*USA):*ISO);

              Exec sql
                Select DAYOFWEEK(:$DATE_A) into :CHECK_DAY
                From SYSIBM.SYSDUMMY1;

              Exec sql
                Select (current date - 3 MONTHS) into :PRE_DATE1
                From SYSIBM.SYSDUMMY1;

              Exec sql
                Select (current date) into :DATE1
                From SYSIBM.SYSDUMMY1;


              DATE2 =  %date(%char(%DATE(MACWDATE : *usa): *iso));

              Select;
                When HDAT1 = *zero and
                  (CHECK_DAY <> 1 and CHECK_DAY <> 7);
                  Exsr $clear_indicater_sr;
                  *in73 = *on;
                  *in13 = *on;
                  Leavesr;

                When DATE2 < PRE_DATE1;
                  Exsr $clear_indicater_sr;
                  *in74 = *on;
                  *in13 = *on;
                  Leavesr;

                When DATE2 > DATE1;
                  Exsr $clear_indicater_sr;
                  *in74 = *on;
                  *in13 = *on;
                  Leavesr;
              Endsl;

              If BFLD = 'MACWDATE';
                Exsr $clear_indicater_sr;
                *in14 = *on;
                Leavesr;
              Endif;

     **  ==> Check_No_Of_Days..........

              Select;
                When MACDAYS = 0;
                  Exsr $clear_indicater_sr;
                  *in54 = *on;
                  *in14 = *on;
                  Leavesr;

                When  BFLD = 'MACDAYS';
                  Exsr $clear_indicater_sr;
                  *in15 = *on;
                  Leavesr;

     **  ==> Check_Reason........

                When MACRSN = *blank;
                  Exsr $clear_indicater_sr;
                  *in55 = *on;
                  *in15 = *on;
                  Leavesr;

                When %check('abcdefghijklmnopqrstuvwxyz ':
                  %trim(MACRSN)) <> 0;
                  Exsr $clear_indicater_sr;
                  *in55 = *on;
                  *in15 = *on;
                  Leavesr;

                When  BFLD = 'MACRSN';
                  Exsr $clear_indicater_sr;
                  *in31 = *on;
                  Leavesr;

                When BFLD = 'SUBMIT';
                  Exsr $clear_indicater_sr;
                  *in31 = *on;
                  *in71 = *on;
                  Leavesr;
              Endsl;
          Endsr;


     **  ==> (5) MANAGER APPLY LEAVES..........

          Begsr $MDAPLYLIV_SR;
            Dow *in03 = *off;
              Malusr = 'MANAGER';
              Exec sql
                Select team into :malteam
                From EMP_MASTER
                Where emp_id = :uid;
              Exsr $uploaddta_sr;
              Exfmt MALEAVE1;

              If *in12 = *on;
                Reset mselect;
                Leavesr;

              Elseif *in05 = *on;
                Reset MALEAVE1;

              Elseif malselect = 1 and MALCL = 0;
                SCS_WIN = '    NOT ENOUGH LEAVE';
                EXFMT SUCCESSWIN ;
              Elseif malselect = 1 and MALCL > 0;
                Exsr $APLCL_SR;

              Elseif malselect = 2 and MALEL = 0;
                SCS_WIN = '    NOT ENOUGH LEAVE';
                EXFMT SUCCESSWIN ;
              Elseif malselect = 2 and MALEL > 0;
                Exsr $APLCL_SR;

              Elseif malselect = 3 and MALCOL = 0;
                SCS_WIN = '    NOT ENOUGH LEAVE';
                EXFMT SUCCESSWIN ;
              Elseif malselect = 3 and MALCOL > 0;
                Exsr $MDAPLYCMP_SR;
              Endif;
            Enddo;
          Endsr;

     **  ==> Manager_Apply_Leaves_Load........

          Begsr $uploaddta_sr;
            Exec sql
              Select z.cl,z.el,z.comp_leave into :MALCL,:MALEL,:MALCOL
              From LEAVEBL_PF z
              Where z.emp_bid = :uid;
            Endsr;

     **  ==> Manager_Apply_leave_cl/el_Subroutine........

          Begsr $APLCL_SR;
            Reset MALEAVE2;
            Reset TTLIVAPLY;
            Dow *in03 = *off;
              If ROLE_1 = 'MANAGER';
                Maplusr = 'MANAGER';
              Else;
                Maplusr = 'EMPLOYEE';
              Endif;
              Exec sql
                Select team into :maplteam from EMP_MASTER
                Where emp_id = :uid;
              Exfmt MALEAVE2;

              If *in12 = *on;
                Reset TTLIVAPLY;
                Reset Malselect;
                Leavesr;

              Elseif *in05 = *on;
                Reset MALEAVE2;
                Reset TTLIVAPLY;

              Else;
                Exsr $chckdatel_sr;
                Select;
                  When *in58 = *off and *in59 = *off and *in60 = *off and
                    *in61 = *off and *in62 = *off and *in72 = *off and
                    *in73 = *off and *in74 = *off and *in75 = *off and
                    *in76 = *off and *in77 = *off and *in78 = *off and
                    *in79 = *off and *in80 = *off and *in71 = *on and
                    *in31 = *on ;

                    If Malselect = 1;
                      LIV_TYP1 = 'CL';
                    Elseif malselect = 2;
                      LIV_TYP1 = 'EL';
                    Endif;

                    Exec sql
                      Insert into LEAVE_PF
                        (EMP_LID , FROM_DATE , FROM_DAY, TO_DATE ,
                        TO_DAY , TOTAL_DAY , REASON , STATUS , LEAVE_TYPE )
                      VALUES
                        ( :UID , :MAPLFDATE , :MAPLFDAY , :MAPLTDATE ,
                        :MAPLTDAY , :NET_CNT , :MAPLRSN , 'P' , :LIV_TYP1 );

                      If Sqlcode = 0;
                        Exec sql
                          Update LEAVEBL_PF set
                            CL = CASE :LIV_TYP1 WHEN 'CL'
                            THEN CL - :NET_CNT
                            ELSE CL END ,
                            EL = CASE :LIV_TYP1 WHEN 'EL'
                            THEN EL - :NET_CNT
                            ELSE EL END
                          Where emp_bid = :uid ;
                        SCS_WIN = '    SUCCESSFULLY APPLIED';
                        Reset MALEAVE2;
                        EXFMT SUCCESSWIN ;
                      Endif;
                Endsl;
              Endif;
            Enddo;
          Endsr;


     **   ************* MANAGER_APPLY_LEAVE_VALIDATION *************

          Begsr $chckdatel_sr;

     **  ==> From_Date_Selection.......
            If MAPLFDATE = *zero;
                  Exsr $clear_indicater_sr;
                  *In58 = *on;
                  *In18 = *on;
                  Leavesr;
            Endif;

            Test(de) *usa MAPLFDATE ;
            If %error;
              Exsr $clear_indicater_sr;
              *in58 = *on;
              *in18 = *on;
              Leavesr;
            Endif;
            $DATE_A = %char(%date(MAPLFDATE:*usa):*iso);
            DATE1 = %date(%char(%date(MAPLFDATE : *usa):*iso));

            Exec sql
              Select DATE into :HDAT1 from HOLIDAY_PF
              Where DATE = :MAPLFDATE;

            Exec sql
              Select DAYOFWEEK(:$DATE_A) into :CHECK_DAY
              From SYSIBM.SYSDUMMY1;

            Exec sql
              Select (current date) into :CUR_DATE
              From SYSIBM.SYSDUMMY1;

            Exec sql
              Select COUNT(1) INTO :LIV_CNT1
                From LEAVE_PF
              Where emp_lid = :uid and
                Status in ('A','P') and
                (from_date = :MAPLFDATE or
                :MAPLFDATE between FROM_DATE  AND TO_DATE );

              Select;
                When HDAT1 <> 0 and
                  (CHECK_DAY = 1 or CHECK_DAY = 7);
                  Exsr $clear_indicater_sr;
                  *In72 = *on;
                  *In18 = *on;
                  Leavesr;

                When DATE1 < CUR_DATE;
                  Exsr $clear_indicater_sr;
                  *In73 = *on;
                  *In18 = *on;
                  Leavesr;

                When LIV_CNT1 > 0;
                  Exsr $clear_indicater_sr;
                  *In74 = *on;
                  *In18 = *on;
                  Leavesr;

                When DFLD = 'MAPLFDATE';
                  Exsr $clear_indicater_sr;
                  *in20 = *on;
                  Leavesr;
              Endsl;



     **  ==> To_Date_Selection.....
              If MAPLTDATE = *zero;
                Exsr $clear_indicater_sr;
                *In60 = *on;
                *In20 = *on;
                Leavesr;
              Endif;

              Test(de) *usa MAPLTDATE ;
              If %error;
                Exsr $clear_indicater_sr;
                *in60 = *on;
                *in20 = *on;
                Leavesr;
              Endif;

              DATE2 =  %date(%char(%DATE(MAPLTDATE : *usa): *iso));
              $DATE_B = %char(%date(MAPLTDATE:*usa):*iso);

              Exec sql
                Select DATE into :HDAT2 from HOLIDAY_PF
                Where DATE = :MAPLTDATE;

              Exec sql
                Select CL,EL into :CL_CNT,:EL_CNT from LEAVEBL_PF
                Where EMP_BID = :UID;

              Exec sql
                Select DAYOFWEEK(:$DATE_B) into :CHECK_DAY1
                From SYSIBM.SYSDUMMY1;

              Exec sql
                Select current date into :CUR_DATE
                From SYSIBM.SYSDUMMY1;

              Exec sql
                Select COUNT(1) INTO :LIV_CNT2
                  From LEAVE_PF
                Where emp_lid = :uid and
                  Status in ('A','P') and
                  (to_date = :MAPLTDATE or
                  :MAPLTDATE  between FROM_DATE  AND TO_DATE );

              Exec sql
                Select COUNT(1) INTO :LIV_CNT3
                  From LEAVE_PF
                Where emp_lid = :uid and
                  status in ('A' , 'P') and
                  (from_date between :MAPLFDATE and :MAPLTDATE );

              Select;
                When HDAT2 <> 0 and
                  (CHECK_DAY1 = 1 or CHECK_DAY1 = 7);
                  Exsr $clear_indicater_sr;
                  *In75 = *on;
                  *In20 = *on;
                  Leavesr;

                When DATE2 < CUR_DATE;
                  Exsr $clear_indicater_sr;
                  *In76 = *on;
                  *In20 = *on;
                  Leavesr;

                When DATE2 < DATE1;
                  Exsr $clear_indicater_sr;
                  *In76 = *on;
                  *In20 = *on;
                  Leavesr;

                When LIV_CNT2 > 0;
                  Exsr $clear_indicater_sr;
                  *In77 = *on;
                  *In20 = *on;
                  Leavesr;

                When LIV_CNT3 > 0;
                  Exsr $clear_indicater_sr;
                  *In77 = *on;
                  *In20 = *on;
                  Leavesr;

                When DFLD = 'MAPLTDATE';
                  Exsr $clear_indicater_sr;
                  *in19 = *on;
                  Leavesr;
              Endsl;

     **  ==> From_Day_Type_Selection...
              Select;
                When DFLD = 'MAPLFDAY' and *in04 = *on;
                  Exfmt daytypwin;
                  *In04 = *off;
                  If Fdy = 1;
                    Maplfday = 'FH';
                    *in21 = *on;
                    Leavesr;
                  Elseif Fdy = 2;
                    Maplfday = 'FD';
                    *in21 = *on;
                    Leavesr;
                  Elseif Fdy = 3;
                    Maplfday = 'SH';
                    *in21 = *on;
                    Leavesr;
                  Endif;

                When MAPLFDAY = *BLANK;
                  Exsr $clear_indicater_sr;
                  *In59 = *on;
                  *In19 = *on;
                  Leavesr;

                When DFLD = 'MAPLFDAY';
                  Exsr $clear_indicater_sr;
                  *in21 = *on;
                  Leavesr;
              Endsl;

     **  ==> To_Day_Type_Selection........

              Select;
                When DFLD = 'MAPLTDAY' and *in04 = *on;
                  EXFMT daytypwin;
                  *in04 = *off;
                  If Fdy = 1;
                    Mapltday = 'FH';
                  Elseif Fdy = 2;
                    Mapltday = 'FD';
                  Elseif Fdy = 3;
                    Mapltday = 'SH';
                  Endif;
              Endsl;
              Select;
                When Mapltday = *BLANK;
                  Exsr $clear_indicater_sr;
                  *in21 = *on;
                  *in61 = *on;
                  Leavesr;

                When MAPLFDATE = MAPLTDATE and MAPLTDAY <> MAPLFDAY;
                  Exsr $clear_indicater_sr;
                  *in21 = *on;
                  *in78 = *on;
                  Leavesr;

                When MAPLFDATE <> MAPLTDATE and MAPLFDAY <> 'FD' and
                   MAPLTDAY <> 'FD' ;
                  Exsr $clear_indicater_sr;
                  *in79 = *on;
                  *in21 = *on;
                  Leavesr;
              Endsl;


     **  ==> Date_Difference_Check & Count......
              DAY_CNT = 0;
              Dow DATE1 <= DATE2;
                $DATE_A = %char(DATE1);
                Exec sql
                  Select DAYOFWEEK(:$DATE_A) into :CHECK_DAY
                    From SYSIBM.SYSDUMMY1;
                If (CHECK_DAY = 1 or CHECK_DAY = 7);
                  DAY_CNT = DAY_CNT + 1;
                Endif;
                DATE1 = (%DATE($DATE_A) + %days(1));
              Enddo;
              DATE1 = %date(%char(%DATE(MAPLFDATE : *usa): *iso));
              GROSS_CNT = %diff(DATE2 : DATE1 : *days)+1-DAY_CNT;

              If MAPLFDAY = 'FD' and MAPLTDAY = 'FD';
                NET_CNT = GROSS_CNT;
              Elseif MAPLFDAY = 'FD' and MAPLTDAY = 'SH';
                NET_CNT = GROSS_CNT;
              Elseif MAPLFDAY = 'FH' and MAPLTDAY = 'FD';
                NET_CNT = GROSS_CNT;
              Else;
                NET_CNT = GROSS_CNT - 0.5;
              Endif;

              TTLIVAPLY = NET_CNT;
              Select;
                When malselect = 1 ;
                  If CL_CNT < NET_CNT;
                    Exsr $clear_indicater_sr;
                    *in80 = *on;
                    *in21 = *on;
                    Leavesr;
                  Endif;

                When malselect = 2 ;
                  If EL_CNT < NET_CNT;
                    Exsr $clear_indicater_sr;
                    *in80 = *on;
                    *in21 = *on;
                    Leavesr;
                  Endif;
              Endsl;

              Select;
                When DFLD = 'MAPLTDAY';
                  Exsr $clear_indicater_sr;
                  *in22 = *on;
                  Leavesr;
              Endsl;

     **  ==> Reason check......
              Select;
                When MAPLRSN = *BLANK;
                  Exsr $clear_indicater_sr;
                  *in62 = *on;
                  *in22 = *on;
                  Leavesr;

                When %check('ABCDEFGHIJKLMNOPQRSTUVWXYZ ' : MAPLRSN) <> 0;
                  Exsr $clear_indicater_sr;
                  *in62 = *on;
                  *in22 = *on;
                  Leavesr;

                When DFLD = 'MAPLRSN';
                    Exsr $clear_indicater_sr;
                    *in31 = *on;
                    Leavesr;

                When DFLD = 'SUBMIT';
                    Exsr $clear_indicater_sr;
                    *in31 = *on;
                    *in71 = *on;
                    Leavesr;

              Endsl;
          Endsr;


     **  ==> 6 MANAGER_LEAVE_STATUS.........

          Begsr $MDLIVSTS_SR;
            Reset mlsts;
            Dow *in03 = *off;
              Mlsusr = 'MANAGER';
              Exsr $clrleave_sr;
              Exsr $lodleave_sr;
              Exec sql select team into :mlsteam from EMP_MASTER
              Where emp_id = :uid;
              Write dltfooter;
              Exsr $dspleave_sr;

              If *in12 = *on;
                Leavesr;

              Elseif *in05 = *on;
                Reset mlsopt;
              Endif;

              Readc MLSTS ;
              Dow not %eof();
                *In02 = *on;

                If Mlsopt = 1 and mlssts = 'P';
                  Exec sql
                    Update LEAVE_PF set
                      STATUS = 'C'
                    Where TO_DATE = :MLSTDATE and
                      emp_lid = :uid;

                  Select;
                    When MLSLTYP = 'CL';
                      Exec sql
                        Update LEAVEBL_PF set
                          CL = CL + :MLSTTLDAY
                        Where emp_bid = :uid;

                    When MLSLTYP = 'EL';
                      Exec sql
                      Update LEAVEBL_PF set
                        EL = EL + :MLSTTLDAY
                      Where emp_bid = :uid;
                  Endsl;
                Endif;
                Readc MLSTS ;
              Enddo;
              *In02 = *off;
              Reset mlsopt;
            Enddo;
          Endsr;

     **  Manager_Leave_Status_Clear........

          Begsr $clrleave_sr;
            *In97 = *off;
            Write MLSTSC;
            *In97 = *on;
            Rrn8 = 0;
          Endsr;

     **  Manager_Leave_Status_Load.........

          Begsr $lodleave_sr;
            Exec sql declare c7 cursor for
              Select FROM_DATE,TO_DATE,TOTAL_DAY,
                STATUS,LEAVE_TYPE
                From LEAVE_PF
              Where emp_lid = :uid;
            Exec sql open c7;
            Exec sql fetch from c7 into :HDAT1,
              :HDAT2,:TOTAL_DAY1,:L_STS,:LIV_TYP1;
            DoW sqlcode = 0;
              rrn8 = rrn8 + 1;
              MLSFDATE = HDAT1;
              MLSTDATE = HDAT2;
              MLSTTLDAY = TOTAL_DAY1;
              MLSSTS = L_STS;
              MLSLTYP = LIV_TYP1;
              Write MLSTS ;
              Exec sql fetch from c7 into :HDAT1,:HDAT2,:TOTAL_DAY1,
                                          :L_STS,:LIV_TYP1;
            Enddo;
            Exec sql close c7;
          Endsr;

     **  Manager leave status display......

          Begsr $dspleave_sr;
            Exfmt MLSTSC ;
          Endsr;


     **  ==> 7 MANAGER_COMP_OFF_STATUS....

          Begsr $MDCMPOSTS_SR ;
            Reset mcsts;
            Dow *in03 = *off;
              Exsr $clrcmpls_sr;
              Exsr $lodcmpls_sr;
              Exec sql select team into :mcsteam from EMP_MASTER
              Where emp_id = :uid;
              Mcsusr = 'MANAGER';
              Exsr $dspcmpls_sr;

              If *in12 = *on;
                Leavesr;

              Elseif *in05 = *on;
                Reset mcsopt;
              Else;

                Readc MCSTS ;
                Dow not %eof();
                  *In02 = *on;
                  If Mcsopt = 1 and mcssts = 'P';
                    Exec sql
                      Update COMP_PF set
                        Status = 'C'
                      Where WORK_DATE = :MCSDATE and
                        emp_cid = :uid;

                    Exec sql
                      Update LEAVEBL_PF set
                        comp_leave = comp_leave + 1
                      Where emp_bid = :uid;
                  Endif;
                  Readc Mcsts ;
                Enddo;
                *In02 = *off;
                Reset mcsopt;
              Endif;
            Enddo;
          Endsr;

     **  Manager_Comp_Off_Status_Clear.....

          Begsr $clrcmpls_sr;
            *In98 = *off;
            Write MCSTSC;
            *In98 = *on;
            Rrn9 = 0;
          Endsr;

     **  Manager comp off status load.....

          Begsr $lodcmpls_sr;
            Exec sql declare c8 cursor for
              Select WORK_DATE,REASON,STATUS
                From COMP_PF
              Where emp_cid = :uid;
            Exec sql open c8;
            Exec sql fetch from c8 into :HDAT1,:REASON_1,:C_STS;
              DoW sqlcode = 0;
                Rrn9 = rrn9 + 1;
                Mcsdate = HDAT1;
                Mcsrsn = REASON_1;
                Mcssts = C_STS;
                Write mcsts ;
                Exec sql fetch from c8 into :HDAT1,:REASON_1,:C_STS;
              Enddo;
            Exec sql close c8;
          Endsr;

     **  Manager_Comp_Off_Status_Display.....

          Begsr $dspcmpls_sr;
            Write dltfooter;
            Exfmt MCSTSC ;
          Endsr;


     **  ==> 8 MANAGER_VIEW_HOLIDAY.......

          Begsr $MDVIWHLD_SR;
            Dow *in03 = *off;
              Exsr $mvhclr_sr;
              Exsr $mvhlod_sr;
              Mvhusr = 'MANAGER';
              Exec sql select team into :mvhteam from EMP_MASTER
              Where emp_id = :uid;
              Exsr $mvhdisply_sr;

              If *in12 = *on;
                Reset mselect;
                Leavesr;
              Endif;
            Enddo;
          Endsr;

     **  Manager_View_Holiday_Clear....

          Begsr $mvhclr_sr;
            *In99 = *off;
            Write MVHLDC;
            *In99 = *on;
            Rrn10 = 0;
          Endsr;

     **  Manager_View_Holiday_Load.....

          Begsr $mvhlod_sr;
            Exec sql declare c9 cursor for
              select date, day, remarks from HOLIDAY_PF;
            Exec sql open c9;
            Exec sql fetch from c9 into :HDAT2, :H_DAY, :REMARK_1;

            DoW sqlcode = 0;
              Rrn10 = rrn10 + 1;
              Mvhdate = HDAT2;
              Mvhday = H_DAY ;
              Mvhrmrk = REMARK_1 ;
              Write MVHLD;
              Exec sql fetch from c9 into :HDAT2, :H_DAY, :REMARK_1;
            Enddo;
            Exec sql close c9;
          Endsr;

     **  Manager_View_Holiday_Display.....

          Begsr $mvhdisply_sr;
            Write dltfooter;
            Exfmt MVHLDC;
          Endsr;


     **  ==> 9 MANAGER_PASSWORD_CHANGE........

          Begsr $MDCHGPASS_SR;
            Dow *in03 = *off;
              Exec sql select team into :mcpteam from EMP_MASTER
              where emp_id = :uid;
              Exec sql select USER_ID,PASSWORD into :MCPUSER,:C_PASSWORD
                From LOGIN_PF
              Where USER_ID = :uid;
              Mcpusr = 'MANAGER';
              Exfmt mcpass;

              If *in12 = *on;
                Reset mcpass;
                Leavesr;
              Elseif *in05 = *on;
                Reset mcpass;
              Else;
                Exsr $checkpass_sr;

                If *in51 = *off and *in52 = *off and
                  *in53 = *off and *in71 = *on and
                  *in31 = *on;
                  Exec sql
                    Update LOGIN_PF set
                      PASSWORD = :mcpnpass ;
                    If sqlcode = 0;
                      SCS_WIN = '    SUCCESSFULLY CHANGED';
                      Reset mcpass;
                      EXFMT SUCCESSWIN ;
                    Endif;
                Endif;
              Endif;
            Enddo;
          Endsr;

     **  Password_Validation_Check........

          Begsr $checkpass_sr;
            If Mcpcpass = *blank;
              Exsr $clear_indicater_sr;
              *In51 = *on;
              *In21 = *on;
              Leavesr;

            Elseif mcpcpass <> C_PASSWORD;
              Exsr $clear_indicater_sr;
              *In51 = *on;
              *In21 = *on;
              Leavesr;

            Elseif efld = 'MCPCPASS';
              Exsr $clear_indicater_sr;
              *In22 = *on;
              leavesr;

            Elseif mcpnpass = *blank;
              Exsr $clear_indicater_sr;
              *In52 = *on;
              *In22 = *on;
              Leavesr;

            Elseif mcpcpass = mcpnpass;
              Exsr $clear_indicater_sr;
              *In52 = *on;
              *In22 = *on;
              Leavesr;

            Elseif efld = 'MCPNPASS';
              Exsr $clear_indicater_sr;
              *In23 = *on;
              leavesr;

            Elseif mcpnpassv = *blank;
              Exsr $clear_indicater_sr;
              *In53 = *on;
              *In23 = *on;
              Leavesr;

            Elseif mcpnpass <> mcpnpassv;
              Exsr $clear_indicater_sr;
              *In53 = *on;
              *In23 = *on;
              Leavesr;

            Elseif efld = 'MCPNPASSV';
              Exsr $clear_indicater_sr;
              *In71 = *on;
              *In31 = *on;
              leavesr;
            Endif;

            Exsr $clear_indicater_sr;
          Endsr;

     ** ******************** END OF MANAGER PAGE **********************
     ** *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
     ** **************** STARTING EMPLOYEE DASHBOARD ******************

          Begsr $EMPLOYEE_SR;
            Dow *in03 = *off;
              Exec sql select team into :edteam from EMP_MASTER
              Where emp_id = :uid;
              Reset edselect;
              Exfmt EMPDSBRD;

              If *In12 = *on;
                EXSR $refresh_sr;
                Leave;

              Elseif *In05 = *on;
                Reset edselect;

              Elseif Edselect = 1;
                Exec sql
                  Select COMP_LEAVE INTO :LIVBL_CNT
                  From LEAVEBL_PF
                  Where emp_bid = :uid;
                If LIVBL_CNT < 1;
                  SCS_WIN = '    NOT ENOUGH LEAVE';
                  Reset Edselect;
                  EXFMT SUCCESSWIN ;
                  LIVBL_CNT = 0;
                Else;
                  Exsr $MDAPLYCMP_SR;
                  LIVBL_CNT = 0;
                Endif;

              Elseif Edselect = 2;
                Exsr $EDAPLYLIV_SR;

              Elseif Edselect = 3;
                Exec sql
                  Select COUNT(*) INTO :LIVBL_CNT
                  From LEAVE_PF
                  Where emp_lid = :uid;
                If LIVBL_CNT < 1;
                  SCS_WIN = '    NOT APPLIED TILL NOW';
                  Reset Edselect;
                  EXFMT SUCCESSWIN ;
                  LIVBL_CNT = 0;
                Else;
                  Exsr $EDLIVSTS_SR;
                  LIVBL_CNT = 0;
                Endif;

              Elseif Edselect = 4;
                Exec sql
                  Select COUNT(*) INTO :CLIV_CNT
                  From COMP_PF
                  Where emp_cid = :uid;
                If CLIV_CNT < 1;
                  SCS_WIN = '   NOT APPLIED TILL NOW';
                  Reset Edselect;
                  EXFMT SUCCESSWIN ;
                  CLIV_CNT = 0;
                Else;
                  Exsr $EDCMPOSTS_SR;
                  CLIV_CNT = 0;
                Endif;

              Elseif Edselect = 5;
                Exec sql
                  Select COUNT(*) INTO :HDAY_CNT
                    From HOLIDAY_PF;
                If HDAY_CNT < 1;
                  SCS_WIN = ' NOTHING IN HOLIDAY RECORDS';
                  Reset Edselect;
                  EXFMT SUCCESSWIN ;
                  HDAY_CNT = 0;
                Else;
                  Exsr $EDVIWHLD_SR;
                  HDAY_CNT = 0;
                Endif;

              Elseif Edselect = 6;
                Exsr $EDCHGPASS_SR;

              Elseif Edselect = 7;
                Leavesr;
              Endif;
            Enddo;
          Endsr;


     **  ==> 2 EMPLOYEE_APPLY_LEAVES..........

          Begsr $EDAPLYLIV_SR;
            Dow *in03 = *off;
              Malusr = 'EMPLOYEE';
              Exec sql
                Select team into :malteam
                From EMP_MASTER
                Where emp_id = :uid;
              Exsr $uplddtaa_sr;
              Reset Malselect;
              Exfmt MALEAVE1;

              If *in12 = *on;
                Reset Edselect;
                Leavesr;

              Elseif *in05 = *on;
                Reset MALEAVE1;

              Elseif malselect = 1 and MALCL = 0;
                SCS_WIN = '    NOT ENOUGH LEAVE';
                Reset Malselect;
                EXFMT SUCCESSWIN ;
              Elseif malselect = 1 and MALCL > 0;
                EXSR $APLCL_SR;

              Elseif malselect = 2 and MALEL = 0;
                SCS_WIN = '    NOT ENOUGH LEAVE';
                Reset Malselect;
                EXFMT SUCCESSWIN ;
              Elseif malselect = 2 and MALEL > 0;
                EXSR $APLCL_SR;

              Elseif malselect = 3 and MALCOL = 0;
                SCS_WIN = '    NOT ENOUGH LEAVE';
                Reset Malselect;
                EXFMT SUCCESSWIN ;
              Elseif malselect = 3 and MALCOL > 0;
                EXSR $MDAPLYCMP_SR;

              Endif;
            Enddo;
          Endsr;

     **  ==> Employee_Apply_Leave_Data_Load......

          Begsr $uplddtaa_sr;
            Exec sql
              Select cl,el,comp_leave into :MALCL,:MALEL,:MALCOL
              From LEAVEBL_PF
              Where emp_bid = :uid;
          Endsr;



     **  ==> (3) EMPLOYEE_LEAVE_STATUS..........

          Begsr $EDLIVSTS_SR;
            Reset mlsts;
            Dow *in03 = *off;
              Exsr $clrleave_sr;
              Exsr $lodleave_sr;
              Mlsusr = 'EMPLOYEE';
              Exec sql select team into :mlsteam from EMP_MASTER
                Where emp_id = :uid;
              Write dltfooter;
              Exsr $dspleave_sr;

              If *in12 = *on;
                Reset Edselect;
                Leavesr;

              Elseif *in05 = *on;
                Reset mlsopt;
              Endif;

              Readc MLSTS ;
              Dow not %eof();
                *In02 = *on;
                If mlsopt = 1 and MLSSTS = 'P';
                  Exec sql
                  Update LEAVE_PF set
                    STATUS = 'C'
                  Where TO_DATE = :MLSTDATE and
                    emp_lid = :uid;
                  Select;
                    When MLSLTYP = 'CL';
                      Exec sql
                        Update LEAVEBL_PF set
                          CL = CL + :MLSTTLDAY
                        Where emp_bid = :uid;

                    When MLSLTYP = 'EL';
                      Exec sql
                        Update LEAVEBL_PF set
                          EL = EL + :MLSTTLDAY
                        Where emp_bid = :uid;
                  Endsl;

                Endif;
                Readc MLSTS ;
              Enddo;
              *In02 = *off;
              Reset mlsopt;
            Enddo;
          Endsr;

     **  ==> (4) EMPLOYEE_COMP_OFF_STATUS.....

          Begsr $EDCMPOSTS_SR ;
            Reset mcsts;
            Dow *in03 = *off;
              Exsr $clrcmpls_sr;
              Exsr $lodcmpls_sr;
              Exec sql select team into :mcsteam from EMP_MASTER
                Where emp_id = :uid;
              Mcsusr = 'EMPLOYEE';
              Write dltfooter;
              Exsr $dspcmpls_sr;

              If *in12 = *on;
                Reset Edselect;
                Leavesr;
              Elseif *in05 = *on;
                Reset mcsopt;
              Endif;

              Readc MCSTS ;
              Dow not %eof();
                *In02 = *on;

                If Mcsopt = 1 and Mcssts = 'P';
                  Exec sql
                    Update COMP_PF set
                      STATUS = 'C'
                    Where WORK_DATE = :MCSDATE;
                  Exec sql
                    Update LEAVEBL_PF set
                      Comp_leave = comp_leave + 1
                      Where emp_bid = :uid;
                Endif;

                Readc MCSTS ;
              Enddo;

              *In02 = *off;
              Reset mcsopt;
            Enddo;
          Endsr;


     **  ==> (5) EMPLOYEE_VIEW_HOLIDAY.......

          Begsr $EDVIWHLD_SR;
            Dow *in03 = *off;
              Exsr $mvhclr_sr;
              Exsr $mvhlod_sr;
              Exec sql select team into :mvhteam
                from EMP_MASTER
                Where emp_id = :uid;
              Mvhusr = 'EMPLOYEE';
              Write dltfooter;
              Exsr $mvhdisply_sr;

              If *in12 = *on;
                Reset Edselect;
                Reset avhopt;
                Leavesr;
              Endif;

            Enddo;
          Endsr;


     **  ==> (6) EMPLOYEE PASSWORD CHANGE........

          Begsr $EDCHGPASS_SR;
            Dow *in03 = *off;
              Exec sql select team into :mcpteam from EMP_MASTER
                Where emp_id = :uid;
              Exec sql select USER_ID,PASSWORD into :MCPUSER,:C_PASSWORD
                from LOGIN_PF
                Where USER_ID = :uid;
              Mcpusr = 'EMPLOYEE';
              Exfmt mcpass;

              If *in12 = *on;
                Reset Edselect;
                Reset mcpass;
                Leavesr;
              ElseIf *in05 = *on;
                Reset mcpass;
              Else;
                Exsr $checkpass_sr;
                If *in51 = *off and *in52 = *off and
                  *in53 = *off and *in71 = *on and
                  *in31 = *on;
                  Exec sql
                    Update LOGIN_PF set
                      PASSWORD = :mcpnpass ;
                      If sqlcode = 0;
                        SCS_WIN = '     SUCCESSFULLY CHANGED';
                        Reset mcpass;
                        EXFMT SUCCESSWIN ;
                      Endif;
                Endif;
              Endif;
            Enddo;
          Endsr;

     **   ****************** EMPLOYEE PAGE ENDED ************************
     **   *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
     **         **************** THANK YOU *******************
