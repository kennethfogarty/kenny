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