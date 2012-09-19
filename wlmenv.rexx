/*REXX*******************************************************************/
/*                                                                      */
/* Exec Name: WLMVER                                                    */
/*                                                                      */
/* Function :                                                           */
/*                                                                      */
/* Returns  :                                                           */
/*                                                                      */
/* Copyright (c)2012 Kenny Fogarty                                      */
/*                                                                      */
/*                                                                      */
/************************************************************************/
                                                    /*                  */
   Trace ('O')                                      /* Trace options    */
                                                    /*                  */
   g.0ExitRc = 0                                    /* Exit code        */
   g.0Debug  = 0                                    /*                  */
                                                    /*                  */   
   Arg wArgs                                        /* Get args         */
   Parse Var wArgs wSubsystem wOptions .            /* Split args       */
                                                    /*                  */   
   If wSubsystem = "" Then wRc = ExitRc(g.0ExitRc)  /*                  */
                                                    /*                  */   
   If Pos("DEBUG"_,wOptions) > 0 Then g.0Debug = 1  /* Debug mode on    */
                                                    /*                  */
   g.0Subsystem     = wSubsystem                    /* Globalise SSID   */
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
	  g.0SchemaInclude                              /*                  */
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
	  g.0WlmExclude                                 /*                  */
  													/*                  */
/************************************************************************/
/* Here we add the fetch only and uncommitted read clauses.             */
/************************************************************************/
													/*                  */
	wQuery = wQuery "FOR FETCH ONLY WITH UR"        /*                  */
													/*                  */
	If g.0Debug Then Do                             /* Debugging        */
	   Say "Generated query follows:"               /*                  */
	   Say wQuery                                   /*                  */
	End                                             /*                  */
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