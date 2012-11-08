/*REXX*******************************************************************/
/*                                                                      */
/* Exec Name: RXWLMVER                                                  */
/*                                                                      */
/* Function : Queries the DB2 catalog for WLM application environments  */
/*            then generates MVS DISPLAY WLM APPLENV= commands to       */
/*            show their status. Executes the generated commands via    */
/*            the REXX/SDSF interface, parses the results, and displays */
/*            the results in a tabulated report.                        */
/*                                                                      */
/* Returns  : Nothing.                                                  */
/*                                                                      */
/* Copyright (c)2012 Kenny Fogarty                                      */
/*                                                                      */
/*                                                                      */
/************************************************************************/
                                                    /*                  */
   Trace ('O')                                      /* Trace options    */
                                                    /*                  */
   Arg wSubsystem wOptions                          /* Get args         */
   If wSubsystem = "" Then wRc = ExitRc(g.0ExitRc)  /*                  */
                                                    /*                  */   
   If Pos("DEBUG"_,wOptions) > 0 Then Do            /* Debug Mode       */
      Say ""                                        /*                  */
	  Say "DEBUG MODE ON"                       /*                  */
	  g.0Debug = 1                              /* Debug mode on    */
   End                                              /*                  */
   Else g.0Debug = 0                                /* Debug mode off   */
                                                    /*                  */
   g.0Subsystem     = wSubsystem                    /* Globalise SSID   */
   g.0ExitRc        = 0                             /* Exit code        */
   g.0SchemaInclude = ""                            /*                  */
   g.0WlmExclude    = ""                            /*                  */
                                                    /*                  */
   Say "The Mighty DB2 Systems Programming Team WLM Environment Checker"
   Say ""                                           /*                  */
   Say "Checking WLM Environments on "||MVSVAR(SYSNAME)
                                                    /*                  */
   wRc = GetSchemaIncludes()                        /* Get includes     */
   wRc = GetWlmExcludes()                           /* Get excludes     */
   wRc = ConstructQuery()                           /* Generate query   */
                                                    /*                  */
   Say "WLM Environment Verification Completed. Return Code "||wRc||"."
   wRc = ExitRc(wRc)                                /* Exit             */
                                                    /*                  */
   Exit                                             /*                  */
                                                    /*                  */
GetSchemaIncludes:                                  /*                  */
                                                    /*                  */
   Procedure Expose g.                              /*                  */
                                                    /*                  */
   wRc = 0                                          /*                  */
   wSchemaInclude = ReadFile(SCHEMADD SCHEMA)       /* Schema includes  */
                                                    /*                  */
/************************************************************************/
/* This simply constructs an in-list that will be used in the query.    */
/************************************************************************/
                                                    /*                  */
   Do Loop = 1 To Words(wSchemaInclude)             /*                  */
      Select                                        /*                  */
         When Loop < 2 Then                         /* Prefix with '('  */
         g.0SchemaInclude = g.0SchemaInclude       ,/*                  */
         ||"("||Word(wSchemaInclude,Loop)||"',"     /*                  */
         When Loop = Words(wSchemaInclude) Then     /*                  */
	 g.0SchemaInclude = g.0SchemaInclude       ,/* Suffix with ')'  */
	 "'"||Word(wSchemaInclude,Loop)||"')"       /*                  */
	 Otherwise                                  /*                  */
	 g.0SchemaInclude = g.0SchemaInclude       ,/* Quote the rest   */
	 "'"||Word(wSchemaInclude,Loop)||"',"       /*                  */
      End                                           /*                  */
   End                                              /*                  */
                                                    /*                  */
Return wRc                                          /*                  */
                                                    /*                  */
GetWlmExcludes:                                     /*                  */
                                                    /*                  */
   Procedure Expose g.                              /*                  */
                                                    /*                  */
   wRc = 0                                          /*                  */
   wWlmExclude = ReadFile(WLMENVDD WLMENV)          /* Read WLM Excludes*/
                                                    /*                  */
/************************************************************************/
/* This simply constructs an in-list that will be used in the query.    */
/************************************************************************/
                                                    /*                  */
   Do Loop = 1 To Words(wWlmExclude)                /*                  */
      Select                                        /*                  */
         When Loop < 2 Then                         /*                  */
         g.0WlmExclude = g.0WlmExclude             ,/*                  */
         ||"('"||Word(wWlmExclude,Loop)||"',"       /* Prefix with '('  */
         When Loop = Words(wWlmExclude) Then        /*                  */
         g.0WlmExclude = g.0WlmExclude             ,/* Suffix with ')'  */
         "'"||Word(wWlmExclude,Loop)||"')"          /*                  */
         Otherwise                                  /*                  */
         g.0WlmExclude = g.0WlmExclude             ,/*                  */
         "'"||Word(wWlmExlcude,Loop)||"',"          /*                  */
      End                                           /*                  */
   End                                              /*                  */
                                                    /*                  */
Return wRc                                          /*                  */
                                                    /*                  */
ConstructQuery:                                     /*                  */
                                                    /*                  */
   Procedure Expose g.                              /*                  */
                                                    /*                  */
/************************************************************************/
/* This is the start of the query                                       */
/************************************************************************/
						    /*                  */
   wQuery = "SELECT DISTINCT WLM_ENVIRONMENT"      ,/*                  */
   "FROM SYSIBM.SYSROUTINES"                        /*                  */
						    /*                  */
/************************************************************************/
/* If we have schemas to include, we include them here.                 */
/************************************************************************/
                                                    /*                  */
   If Words(g.0SchemaInclude) <> 0 Then            ,/*                  */
      wQuery = wQuery "WHERE SCHEMA IN"            ,/*                  */
	  g.0SchemaInclude                          /*                  */
	                                            /*                  */
/************************************************************************/
/* If we have schemas AND wlm application environments, add the AND     */
/************************************************************************/
   If Words(g.0SchemaInclude) > 0 &                ,/*                  */
      Words(g.0WlmExclude) > 0 Then                ,/*                  */
      wQuery = wQuery "AND"                         /*                  */
                                                    /*                  */	  
/************************************************************************/
/* If we have WLM Application Environments to exclude, we add them here.*/
/************************************************************************/
                                                    /*                  */
   If Words(g.0WlmExclude) <> 0 Then               ,/*                  */
      wQuery = wQuery "WLM_ENVIRONMENT NOT IN"     ,/*                  */
	  g.0WlmExclude                             /*                  */
  						    /*                  */
/************************************************************************/
/* Here we add the fetch only and uncommitted read clauses.             */
/************************************************************************/
						    /*                  */
	wQuery = wQuery "FOR FETCH ONLY WITH UR"    /*                  */
						    /*                  */
	If g.0Debug Then Do                         /* Debugging        */
	   Say "Generated query follows:"           /*                  */
	   Say wQuery                               /*                  */
	End                                         /*                  */
						    /*                  */
/************************************************************************/
/* Create the REXX/DB2 host environment                                 */
/************************************************************************/
						    /*                  */
   Address TSO "SUBCOM DSNREXX"                     /* Db2 env set up?  */
   wRc = rc                                         /* Get return code  */
						    /*                  */
   If wRc Then Do                                   /* No               */
      wRc = RXSUBCOM('ADD','DSNREXX','DSNREXX')     /* Create one.      */
   End                                              /*                  */
						    /*                  */
   Address DSNREXX "CONNECT" g.0Subsystem           /* Connect to DB2   */
   If SQLCODE <> 0 Then Call ShowSQLCA              /*                  */
						    /*                  */
   wRc = SubmitQuery(wQuery)                        /* Submit query     */
   Address DSNREXX "DISCONNECT"                     /* Disconnect       */
						    /*                  */
Return wRc                                          /*                  */
						    /*                  */
SubmitQuery:                                        /*                  */
						    /*                  */
   Arg wQuery                                       /* Get query        */
                                                    /*                  */
   Address DSNREXX                                  /*                  */
   "EXECSQL DECLARE C1 CURSOR FOR S1"               /* Declare cursor   */
   If SQLCODE <> 0 Then Call ShowSQLCA              /* Error received   */
                                                    /*                  */
   Address DSNREXX                                  /*                  */
   "EXECSQL PREPARE S1 INTO :OUT FROM :wQuery"      /* Prepare output   */
   If SQLCODE <> 0 Then Call ShowSQLCA              /* Error received   */
                                                    /*                  */
   Address DSNREXX                                  /*                  */
   "EXECSQL OPEN C1"                                /* Open C1 cursor   */
   If SQLCODE <> 0 Then Call ShowSQLCA              /* Error received   */
                                                    /*                  */
   Do Forever                                       /* Loop             */
      Address DSNREXX                               /*                  */
	   "EXECSQL FETCH C1 USING DESCRIPTOR :OUT" /* Fetch next row   */
	   If SQLCODE = 0 Then Do                   /* All is good      */
	                                            /*                  */
/************************************************************************/
/* Process the fetched row of information.                              */
/************************************************************************/
                                                    /*                  */
         If g.0Debug Then Do                        /* Debugging        */
            Say "ROW RETURNED "||Strip(OUT.1.SQLDATA)
         End                                        /*                  */
         wWlmEnv = Strip(OUT.1.SQLDATA)             /* WLM Environment  */
         If Strip(wWlmENv) = "" Then Iterate        /* Blank wlm_env    */
         wMvsCommand = "/D WLM,APPLENV="||Strip(wWlmEnv)
         wRc = ExecuteCommand(wMvsCommand)          /* Execute command  */
      End                                           /*                  */
                                                    /*                  */
      If SQLCODE = 100 Then Leave                   /* No more results  */
                                                    /*                  */
      If SQLCODE <> 0 & SQLCODE <> 100 Then Do      /* Any other SQL    */
         Call ShowSQLCA                             /* errors           */
          wRc = 8                                   /* Set return code  */
         Leave                                      /* Get out of loop  */
      End                                           /*                  */
   End                                              /*                  */
                                                    /*                  */
   Address DSNREXX                                  /*                  */
   "EXECSQL CLOSE C1"                               /* Close cursor C1  */
   If SQLCODE <> 0 THen Call ShowSQLCA              /*                  */
                                                    /*                  */
Return wRc                                          /*                  */
                                                    /*                  */
ExecuteCommand:                                     /*                  */
                                                    /*                  */
   Procedure Expose g.                              /*                  */
                                                    /*                  */
   Arg wCommand                                     /*                  */
                                                    /*                  */
   If g.0Debug Then Do                              /*                  */
      "WILL EXECUTE THIS COMMAND "||wCommand        /*                  */
   End                                              /*                  */
                                                    /*                  */
   wRc = 0                                          /*                  */
   wMaxCC = 0                                       /*                  */
                                                    /*                  */
   wRc = ISFCALLS('ON')                             /* SDSF interface   */
   ISFCONS = g.0ExecName                            /* Set SDSF console */
                                                    /*                  */
   Address SDSF "ISFEXEC '"||wCommand||"' (WAIT)"   /* Execute command  */
   wRc = rc                                         /*                  */
                                                    /*                  */
   If wRc < 5 Then Do                               /*                  */
      Do Loop = 1 To ISFULOG.0                      /* Parse msg output */
         If g.0Debug Then Say ISFULOG.Loop          /* Debugging        */
         LastCC = 0                                 /* Init LastCC      */
         If Pos("ISF032I",ISFULOG.Loop) > 0 Then Do /*                  */
            LastCC = 4                              /* Set LastCC       */
            Parse Var ISFULOG.Loop . . . . wMsg     /*                  */
            Say Strip(wMsg)                         /*                  */
         End                                        /*                  */
         If Pos("IWM029I",ISFULOG.Loop) > 0 Then Do /*                  */
            LastCC = 0                              /* Set LastCC       */
                                                    /*                  */
            wPointer = Loop                         /* Set pointer      */
            wPointer wPointer + 2                   /* Increment        */
            Parse Var ISFULOG.wPointer             ,/* Parse the message*/
            wEnvironment wState                     /* Get env and state*/ 
            Say "Environment Name: "               ,/*                  */
            Left(wEnvironment,28)||" State:"       ,/*                  */
            Left(Strip(wState),20)||" CC="||LastCC||"."
         End                                        /*                  */
         If Pos("IWM030I",ISFULOG.Loop) > 0 Then Do /*                  */
            LastCC = 8                              /* Set LastCC       */
            Parse Var ISFULOG.Loop . . . . . .     ,/*                  */
            wEnvironment . . . wState               /*                  */
            Say "Environment Name: "               ,/*                  */
            Left(wEnvironment,28)||"State:"        ,/*                  */
            Left(Strip(wState),20)||" CC="||LastCC||"."
         End                                        /*                  */
         If LastCC > MaxCC Then MaxCC = LastCC      /* Update MaxCC     */
      End                                           /*                  */
   End                                              /*                  */
   If wRc > 4 Then Do                               /*                  */
      Say "Command "||wCommand||" failed."          /*                  */
      "REASON CODE="||wRc||"."                      /*                  */
      MaxCC = 8                                     /*                  */
   End                                              /*                  */
   wRc = ISFCALLS('OFF')                            /* Terminate SDSF   */
                                                    /*                  */
Return MaxCC                                        /*                  */
                                                    /*                  */
/************************************************************************/
/* Show the SQLCA                                                       */
/************************************************************************/
                                                    /*                  */
ShowSQLCA:                                          /*                  */
                                                    /*                  */
   Numeric Digits 80                                /*                  */
                                                    /*                  */
   If Right(SQLERRMC,1) <> 'FF'X Then Do            /*                  */
      SQLERRMC = Left(SQLERRMC,69) || 'FF'X         /*                  */
   End                                              /*                  */
                                                    /*                  */
   If SQLWARNA <> 'W' | SQLWARNA = 'SQLWARNA' Then ,/*                  */
      SQLWARNA = ' '                                /*                  */
                                                    /*                  */
   SQLCA = 'SQLCA   '||,                            /*                  */
   X2C(D2X(136,8))||,                               /*                  */
   X2C(D2X(SQLCODE,8))||,                           /*                  */
   X2C(D2X(Length(SQLERRMC),4))||,                  /*                  */
        SQLERRMC        ||,                         /*                  */
   Left(SQLERRP,8)||,                               /*                  */
   X2C(D2X(SQLERRD.1,8))||,                         /*                  */
   X2C(D2X(SQLERRD.2,8))||,                         /*                  */
   X2C(D2X(SQLERRD.3,8))||,                         /*                  */
   X2C(D2X(SQLERRD.4,8))||,                         /*                  */
   X2C(D2X(SQLERRD.5,8))||,                         /*                  */
   X2C(D2X(SQLERRD.6,8))||,                         /*                  */   
   Left(SQLWARN.1,1)||,                             /*                  */
   Left(SQLWARN.1,2)||,                             /*                  */
   Left(SQLWARN.1,3)||,                             /*                  */
   Left(SQLWARN.1,4)||,                             /*                  */
   Left(SQLWARN.1,5)||,                             /*                  */
   Left(SQLWARN.1,6)||,                             /*                  */
   Left(SQLWARN.1,7)||,                             /*                  */
   Left(SQLWARN.1,8)||,                             /*                  */
   Left(SQLWARN.1,9)||,                             /*                  */
   Left(SQLWARN.1,10)||,                            /*                  */                        
   Left(SQLWARNA.1)||,                              /*                  */
   Left(SQLSTATE,5)||,                              /*                  */
   MESSAGE_LLEN = 79                                /*                  */
   MESSAGE_LINES = 12                               /*                  */
   MESSAGE_AREA = X2C(D2X(MESSAGE_LLEN*MESSAGE_LINES,4))||,
   Copies(' ',MESSAGE_LLEN*MESSAGE_LINES)           /*                  */
   MESSAGE_LRECL = X2C(D2X(MESSAGE_LLEN,8))         /*                  */
   Address LINKPGM "DSNTIAR SQLCA MESSAGE_AREA MESSAGE_LRECL"
   MESSAGE_LINE_START = 3                           /*                  */
                                                    /*                  */
   Do Loop = 1 To MESSAGE_LINES                     /*                  */
      MESSAGE.LINE = ,                              /*                  */
      Substr(MESSAGE_AREA,MESSAGE_LINE_START,MESSAGE_LLEN)
      MESSAGE_LINE_START = MESSAGE_LINE_START + MESSAGE_LLEN
      If MESSAGE_LINE <> '' Then Do                 /*                  */
         Say Message_Line                           /*                  */
      End                                           /*                  */
   End                                              /*                  */
Return                                              /*                  */
                                                    /*                  */
ExitRC:                                             /*                  */
                                                    /*                  */
   Arg wRc                                          /*                  */
   Exit wRc                                         /*                  */
                                                    /*                  */
Return 0                                            /*                  */
                                                    /*                  */
ReadFile:                                           /*                  */
                                                    /*                  */
/************************************************************************/
/* Generic routine to read a file into a stem variable called wtemp.    */
/************************************************************************/
   Procedure Expose g.                              /*                  */
                                                    /*                  */
   Arg wDDname wDirective                           /* Get args         */
                                                    /*                  */
   wRc = 0                                          /* Set return code  */
                                                    /*                  */
   wSchemaInclude = ""                              /* Init             */
   wWlmExclude = ""                                 /* Init             */
                                                    /*                  */
   Address TSO                                      /* Read file        */
   "EXECIO * DISKR "||wDDname||" (STEM Temp. FINIS)"/* into wTemp       */
   wRc = rc                                         /* Get return code  */
                                                    /*                  */
   If wRc <> 0 Then Call ExitRc(wRc)                /* Error            */
                                                    /*                  */
   Do Loop = 1 To Temp.0                            /*                  */
      Temp.Loop = Strip(Temp.Loop)                  /* Remove blanks    */
      If Substr(Temp.Loop,1,1) = "#" Then Iterate   /* ignore comments  */
                                                    /*                  */
      Temp.Loop = Word(Temp.Loop,1)                 /* Get first word   */
                                                    /*                  */
/************************************************************************/
/* depending on the argument 'directive' we can either add the word     */
/* to the wSchemaInclude string, or to the wWlmExclude string.          */
/************************************************************************/
                                                    /*                  */
      Select                                        /*                  */
         When wDirective = "SCHEMA" Then Do         /* Include          */
            wSchemaInclude = wSchemaInclude Temp.Loop
         End                                        /*                  */
         When wDirective = "WLMENV" Then Do         /* Exclude          */
            wWlmExclude = wWlmExclude Temp.Loop     /*                  */
         End                                        /*                  */
      End                                           /*                  */
                                                    /*                  */
/************************************************************************/
/* Either return the include or exclude string depending on the value   */
/* of wDirective.                                                       */
/************************************************************************/

   If wDirective = "SCHEMA" Then Return wSchemaInclude
                            Else Return wWlmExclude                             