/*
 * Auto WGet Daemon Support Library
 * Copyright (C) 1998-2005 Dmitry A.Steklenev
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. All advertising materials mentioning features or use of this
 *    software must display the following acknowledgment:
 *    "This product includes software developed by Dmitry A.Steklenev".
 *
 * 4. Redistributions of any form whatsoever must retain the following
 *    acknowledgment:
 *    "This product includes software developed by Dmitry A.Steklenev".
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR OR CONTRIBUTORS "AS IS"
 * AND ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * AUTHOR OR THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id$
 */

#define  INCL_REXXSAA
#define  INCL_WINWORKPLACE
#define  INCL_DOS
#define  INCL_ERRORS
#define  INCL_WIN
#include <os2.h>

#include <rexxsaa.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>

#include "awget.h"
#include "url.h"

#define HF_STDIN  0
#define HF_STDOUT 1
#define HF_STDERR 2

static PSZ AwFncTable[] =
{
  "AwDropFuncs"    ,
  "AwGetObjectPath",
  "AwTQCreate"     ,
  "AwTQClose"      ,
  "AwTQRead"       ,
  "AwStartSession" ,
  "AwStopSession"  ,
  "AwPOpen"        ,
  "AwPKill"        ,
  "AwPClose"       ,
  "AwPRead"        ,
  "AwPWrite"       ,
  "AwQueryURL"     ,
  "AwShow"         ,
  "AwPgmType"      ,
  "AwTimeStamp"    ,
  "AwIsPM"
};

RexxFunctionHandler AwLoadFuncs;
RexxFunctionHandler AwDropFuncs;
RexxFunctionHandler AwGetObjectPath;
RexxFunctionHandler AwTQCreate;
RexxFunctionHandler AwTQClose;
RexxFunctionHandler AwTQRead;
RexxFunctionHandler AwStartSession;
RexxFunctionHandler AwStopSession;
RexxFunctionHandler AwPOpen;
RexxFunctionHandler AwPKill;
RexxFunctionHandler AwPClose;
RexxFunctionHandler AwPRead;
RexxFunctionHandler AwPWrite;
RexxFunctionHandler AwQueryURL;
RexxFunctionHandler AwShow;
RexxFunctionHandler AwPgmType;
RexxFunctionHandler AwTimeStamp;
RexxFunctionHandler AwIsPM;

#define INVALID_ROUTINE 40 /* Raise Rexx error      */
#define VALID_ROUTINE    0 /* Successful completion */

#define ERROR_RETSTR "ERROR:"
#define READY_RETSTR "READY:"
#define ERROR_LENSTR 6
#define READY_LENSTR 6

#define QUEUE_PREFIX "\\QUEUES\\AWGET\\"

/**
 * AwDropFuncs()
 *
 * Use this function to drop all the loaded AWGet functions.
 * Once this function is processed by a REXX program, the AWGet
 * functions are not accessible in any OS/2 sessions.
 */

ULONG AwDropFuncs( PUCHAR    name,
                   ULONG     numargs,
                   RXSTRING  args[],
                   PSZ       queuename,
                   PRXSTRING retstr     )
{
  int i;

  if( numargs != 0 )          /* no arguments for this       */
    return INVALID_ROUTINE;   /* raise an error              */

  for( i = 0; i < sizeof(AwFncTable)/sizeof(PSZ); i++ )
    RexxDeregisterFunction( AwFncTable[i] );

  RexxDeregisterFunction( "AwLoadFuncs" );

  retstr->strlength = 0;      /* return a null string result */
  return VALID_ROUTINE;       /* no error on call            */
}

/**
 * AwLoadFuncs()
 *
 * Will automatically load all AWGet functions. Once the AWGet
 * functions are loaded by SysLoadFuncs they are usable by
 * all OS/2 sessions.
 */

ULONG AwLoadFuncs( PUCHAR    name,
                   ULONG     numargs,
                   RXSTRING  args[],
                   PSZ       queuename,
                   PRXSTRING retstr     )
{
  INT i;

  if( numargs != 0 )          /* no arguments for this       */
    return INVALID_ROUTINE;   /* raise an error              */

  for( i = 0; i < sizeof(AwFncTable)/sizeof(PSZ); i++ )
    RexxRegisterFunctionDll( AwFncTable[i], "AWGET", AwFncTable[i] );

  retstr->strlength = 0;      /* return a null string result */
  return VALID_ROUTINE;       /* no error on call            */
}

/**
 * AwGetObjectPath( object_id )
 *
 * This function returns the directory specification of a given
 * WPS object or empty string if a error occured.
 */

ULONG AwGetObjectPath( PUCHAR    name,
                       ULONG     numargs,
                       RXSTRING  args[],
                       PSZ       queuename,
                       PRXSTRING retstr     )
{
  HOBJECT hObject;
  CHAR    szPath[CCHMAXPATH + 1] = "";
  BOOL    fSuccess;

  if( numargs != 1 )
    return INVALID_ROUTINE;

  hObject = WinQueryObject(args[0].strptr);

  if( hObject )
    fSuccess = WinQueryObjectPath( hObject, szPath, sizeof(szPath));

  strcpy( retstr->strptr, szPath );
  retstr->strlength = strlen( retstr->strptr );
  return VALID_ROUTINE;
}

/**
 * AwTQCreate( name, handle )
 *
 * Creates termination queue. At successful execution queue handle
 * will be placed in the rexx variable. In case of failure, one of
 * many return codes is given so that the exact reason for
 * failure can be determined.
 */

ULONG AwTQCreate( PUCHAR    name,
                  ULONG     numargs,
                  RXSTRING  args[],
                  PSZ       queuename,
                  PRXSTRING retstr     )
{
  APIRET   rc;
  HQUEUE   hqueue;
  SHVBLOCK shv;
  HEV      hev;
  char     buffer[32];
  char     queue_name[_MAX_PATH] = QUEUE_PREFIX;

  if( numargs != 2 )
    return INVALID_ROUTINE;

  strncpy( queue_name + sizeof(QUEUE_PREFIX) - 1,
           args[0].strptr,
          _MAX_PATH - sizeof(QUEUE_PREFIX));

  rc = DosCreateEventSem( NULL, &hev, DC_SEM_SHARED, 0 );

  if( rc == NO_ERROR )
  {
    rc = DosCreateQueue( &hqueue,
                         QUE_FIFO | QUE_CONVERT_ADDRESS, queue_name );

    if( rc == NO_ERROR )
    {
      shv.shvnext = NULL;
      shv.shvname = args[1];
      shv.shvcode = RXSHV_SYSET;
      shv.shvret  = 0;

      sprintf( buffer, "%u:%u", hqueue, hev );
      shv.shvvaluelen = strlen(buffer);

      MAKERXSTRING( shv.shvvalue, buffer, shv.shvvaluelen );

      if( RexxVariablePool( &shv ) == RXSHV_BADN )
        return INVALID_ROUTINE;
    }
  }

  ltoa( rc, retstr->strptr, 10 );
  retstr->strlength = strlen( retstr->strptr );
  return VALID_ROUTINE;
}

/**
 * AwTQClose( handle )
 *
 * Closes termination queue. In case of failure, one of
 * many return codes is given so that the exact reason for
 * failure can be determined.
 */

ULONG AwTQClose( PUCHAR    name,
                 ULONG     numargs,
                 RXSTRING  args[],
                 PSZ       queuename,
                 PRXSTRING retstr     )
{
  APIRET rc;
  HQUEUE hqueue;
  HEV    hev;

  if( numargs != 1 )
    return INVALID_ROUTINE;

  sscanf( args[0].strptr, "%u:%u", &hqueue, &hev );

  if(( rc = DosCloseEventSem( hev )) != NO_ERROR )
    rc = DosCloseQueue( hqueue );

  ltoa( rc, retstr->strptr, 10 );
  retstr->strlength = strlen( retstr->strptr );
  return VALID_ROUTINE;
}

/**
 * AwTQRead( handle, stem )
 *
 * Read termination queue and place results (ses_id ses_rc) in a stem
 * variable. In case of failure, one of many return codes is given
 * so that the exact reason for failure can be determined.
 */

ULONG AwTQRead( PUCHAR    name,
                ULONG     numargs,
                RXSTRING  args[],
                PSZ       queuename,
                PRXSTRING retstr     )
{
  typedef struct {

    USHORT ses_id;
    USHORT ses_rc;
  } CHILDINFO;

  REQUESTDATA rdRequest = {0,0};  /* Request data for the queue */
  ULONG       ulSzData  = 0;      /* Size of the queue data     */
  PVOID       pvData;             /* Pointer to the queue data  */
  BYTE        bPriority = 0;      /* For the queue              */

  APIRET   rc;
  HQUEUE   hqueue;
  SHVBLOCK shv;
  HEV      hev;

  char     stem[256], stem_name[256], stem_value[ 16];
  ULONG    stem_count = 0;

  if( numargs != 2 || !RXVALIDSTRING(args[1]))
    return INVALID_ROUTINE;

  sscanf( args[0].strptr, "%u:%u", &hqueue, &hev );
  strcpy( stem, args[1].strptr );

  if( stem[strlen(stem)-1] != '.' )
    strcat( stem, "." );

  while(( rc = DosReadQueue( hqueue, &rdRequest, &ulSzData, &pvData,
                             0, DCWW_NOWAIT, &bPriority, hev )) == NO_ERROR )
  {
    sprintf( stem_name, "%s%d.%s", stem, ++stem_count, "ses_id" );

    shv.shvnext = NULL;
    shv.shvcode = RXSHV_SYSET;
    shv.shvret  = 0;

    ltoa( ((CHILDINFO*)pvData)->ses_id, stem_value, 10 );
    shv.shvvaluelen = strlen(stem_value);

    MAKERXSTRING( shv.shvvalue, stem_value, shv.shvvaluelen );
    MAKERXSTRING( shv.shvname , stem_name , strlen(stem_name));

    if( RexxVariablePool( &shv ) == RXSHV_BADN )
      return INVALID_ROUTINE;

    sprintf( stem_name, "%s%d.%s", stem, stem_count, "ses_rc" );

    shv.shvnext = NULL;
    shv.shvcode = RXSHV_SYSET;
    shv.shvret  = 0;

    ltoa( ((CHILDINFO*)pvData)->ses_rc, stem_value, 10 );
    shv.shvvaluelen = strlen(stem_value);

    MAKERXSTRING( shv.shvvalue, stem_value, shv.shvvaluelen );
    MAKERXSTRING( shv.shvname , stem_name , strlen(stem_name));

    if( RexxVariablePool( &shv ) == RXSHV_BADN )
      return INVALID_ROUTINE;

    DosFreeMem( pvData );
  }

  sprintf( stem_name, "%s0", stem );

  shv.shvnext = NULL;
  shv.shvcode = RXSHV_SYSET;
  shv.shvret  = 0;

  ltoa( stem_count, stem_value, 10 );
  shv.shvvaluelen = strlen(stem_value);

  MAKERXSTRING( shv.shvvalue, stem_value, shv.shvvaluelen );
  MAKERXSTRING( shv.shvname , stem_name , strlen(stem_name));

  if( RexxVariablePool( &shv ) == RXSHV_BADN )
    return INVALID_ROUTINE;

  if( rc == ERROR_QUE_EMPTY )
    rc = 0;

  ltoa( rc, retstr->strptr, 10 );
  retstr->strlength = strlen( retstr->strptr );
  return VALID_ROUTINE;
}

/**
 * AwStartSession( prog_title, prog_name, prog_parm, options, termq, ses_id )
 *
 * Allows an application to start another session, and to specify the
 * name of the program to be started in that session. In case of failure,
 * one of many return codes is given so that the exact reason for
 * failure can be determined.
 */

ULONG AwStartSession( PUCHAR    name,
                      ULONG     numargs,
                      RXSTRING  args[],
                      PSZ       queuename,
                      PRXSTRING retstr     )
{
  STARTDATA sd;
  ULONG     ulSession;
  PID       pid;
  APIRET    rc;
  SHVBLOCK  shv;

  char      queue_name[_MAX_PATH] = QUEUE_PREFIX;
  char      buffer[16];

  if( numargs < 2 || numargs > 6 )
    return INVALID_ROUTINE;

  memset( &sd, 0, sizeof(sd));

  sd.Length     = sizeof(sd);
  sd.Related    = SSF_RELATED_CHILD;
  sd.InheritOpt = SSF_INHERTOPT_PARENT;

  if( RXVALIDSTRING( args[0] ))
    sd.PgmTitle  = args[0].strptr;

  if( RXVALIDSTRING( args[1] ))
    sd.PgmName   = args[1].strptr;

  if( RXVALIDSTRING( args[2] ))
    sd.PgmInputs = args[2].strptr;

  if( RXVALIDSTRING( args[3] ))
  {
    strupr( args[3].strptr );

    if( strchr( args[3].strptr, 'F' ) != NULL )
      sd.FgBg = SSF_FGBG_FORE;

    if( strchr( args[3].strptr, 'B' ) != NULL )
      sd.FgBg = SSF_FGBG_BACK;

    if( strchr( args[3].strptr, 'H' ) != NULL )
      sd.PgmControl = SSF_CONTROL_INVISIBLE;

    if( strchr( args[3].strptr, 'M' ) != NULL )
      sd.PgmControl = SSF_CONTROL_MINIMIZE;
  }

  if( RXVALIDSTRING( args[4] ))
  {
    strncpy( queue_name + sizeof(QUEUE_PREFIX) - 1,
             args[4].strptr,
             _MAX_PATH - sizeof(QUEUE_PREFIX));

    sd.TermQ = queue_name;
  }

  rc = DosStartSession( &sd, &ulSession, &pid );

  if( rc == NO_ERROR )
  {
    shv.shvnext = NULL;
    shv.shvname = args[5];
    shv.shvcode = RXSHV_SYSET;
    shv.shvret  = 0;

    ltoa( ulSession, buffer, 10 );
    shv.shvvaluelen = strlen(buffer);

    MAKERXSTRING( shv.shvvalue, buffer, shv.shvvaluelen );

    if( RexxVariablePool( &shv ) == RXSHV_BADN )
      return INVALID_ROUTINE;
  }

  ltoa( rc, retstr->strptr, 10 );
  retstr->strlength = strlen( retstr->strptr );
  return VALID_ROUTINE;
}

/**
 * AwStopSession( ses_id )
 *
 * Allows an application to ends one child sessions.
 *
 * AwStopSession may only be issued for a child session. Neither
 * the parent session itself nor any grandchild, nor any other
 * descendant session beyond a child session, may be the target of
 * this function. AwStopSession may only be issued by the process
 * that originally started the specified session (ses_id) with
 * AwStartSession.
 */

ULONG AwStopSession( PUCHAR    name,
                     ULONG     numargs,
                     RXSTRING  args[],
                     PSZ       queuename,
                     PRXSTRING retstr     )
{
  ULONG  ulSession;
  APIRET rc;

  if( numargs != 1 )
    return INVALID_ROUTINE;

  ulSession = atol(args[0].strptr);
  rc = DosStopSession( STOP_SESSION_SPECIFIED, ulSession );

  ltoa( rc, retstr->strptr, 10 );
  retstr->strlength = strlen( retstr->strptr );
  return VALID_ROUTINE;
}

/**
 * AwPOpen( prog_name, prog_parm, handle [, background] )
 *
 * Allows an application to start another program, and
 * to read a output of the program with the help of
 * function AwPRead.
 */

ULONG AwPOpen( PUCHAR    name,
               ULONG     numargs,
               RXSTRING  args[],
               PSZ       queuename,
               PRXSTRING retstr     )
{
  HFILE  hfSaveStd, hfCurrStd,
         hfSaveErr, hfCurrErr,
         hfSaveIn , hfCurrIn ,
         wrPipeOut, rdPipeOut,
         wrPipeIn , rdPipeIn ;

  CHAR   szFailName[CCHMAXPATH];
  APIRET rc;
  char*  szExec;
  char   buffer[64];

  RESULTCODES resCodes;
  SHVBLOCK    shv;

  ULONG pipe_state;

  if( numargs < 3 || numargs > 4 )
    return INVALID_ROUTINE;

  szExec = (char*)malloc( args[0].strlength +
                          args[1].strlength + 1 );
  if( !szExec )
    return INVALID_ROUTINE;

  strcpy( szExec,  args[0].strptr );
  strcpy( szExec + args[0].strlength + sizeof(char), args[1].strptr );

  hfSaveStd = -1;
  hfSaveErr = -1;
  hfSaveIn  = -1;
  hfCurrIn  = HF_STDIN;
  hfCurrStd = HF_STDOUT;
  hfCurrErr = HF_STDERR;

  DosDupHandle ( HF_STDIN , &hfSaveIn  );
  DosDupHandle ( HF_STDOUT, &hfSaveStd );
  DosDupHandle ( HF_STDERR, &hfSaveErr );

  DosCreatePipe( &rdPipeOut, &wrPipeOut, 255 );
  DosCreatePipe( &rdPipeIn , &wrPipeIn , 255 );

  DosSetFHState( rdPipeOut, OPEN_FLAGS_NOINHERIT );
  DosSetFHState( wrPipeIn , OPEN_FLAGS_NOINHERIT );

  DosDupHandle ( wrPipeOut, &hfCurrErr );
  DosDupHandle ( wrPipeOut, &hfCurrStd );
  DosDupHandle ( rdPipeIn , &hfCurrIn  );

  rc = DosExecPgm( szFailName, sizeof(szFailName ),
                   numargs == 4 ? EXEC_BACKGROUND : EXEC_ASYNCRESULT,
                   szExec,
                   (PSZ)NULL, &resCodes, args[0].strptr );

  DosClose    ( wrPipeOut );
  DosClose    ( rdPipeIn  );
  DosDupHandle( hfSaveStd, &hfCurrStd );
  DosDupHandle( hfSaveErr, &hfCurrErr );
  DosDupHandle( hfSaveIn , &hfCurrIn  );
  DosClose    ( hfSaveStd );
  DosClose    ( hfSaveErr );
  DosClose    ( hfSaveIn  );

  free( szExec );

  if( rc == NO_ERROR )
  {
    shv.shvnext = NULL;
    shv.shvname = args[2];
    shv.shvcode = RXSHV_SYSET;
    shv.shvret  = 0;

    sprintf( buffer, "%u:%u:%u", resCodes.codeTerminate, rdPipeOut, wrPipeIn );
    shv.shvvaluelen = strlen(buffer);

    MAKERXSTRING( shv.shvvalue, buffer, shv.shvvaluelen );

    if( RexxVariablePool( &shv ) == RXSHV_BADN )
      return INVALID_ROUTINE;
  }

  ltoa( rc, retstr->strptr, 10 );
  retstr->strlength = strlen( retstr->strptr );
  return VALID_ROUTINE;
}

/**
 * AwPRead( handle )
 *
 * Reads a standard and error output of the program started
 * via AwPOpen. Returns an empty string in case the program
 * was finished. You should use AwPClose after end of
 * the program.
 */

ULONG AwPRead( PUCHAR    name,
               ULONG     numargs,
               RXSTRING  args[],
               PSZ       queuename,
               PRXSTRING retstr     )
{
  PID   pid;
  HFILE pipe_stdout, pipe_stdin;

  if( numargs != 1 )
    return INVALID_ROUTINE;

  sscanf( args[0].strptr, "%u:%u:%u", &pid, &pipe_stdout, &pipe_stdin );
  DosRead( pipe_stdout, (PVOID)retstr->strptr, 255, &retstr->strlength );
  return VALID_ROUTINE;
}

/**
 * AwPWrite( handle, string )
 *
 * Write to a standard input of the program started
 * via AwPOpen. Returns the number of bytes actually
 * written.
 */

ULONG AwPWrite( PUCHAR    name,
                ULONG     numargs,
                RXSTRING  args[],
                PSZ       queuename,
                PRXSTRING retstr     )
{
  PID   pid;
  HFILE pipe_stdout, pipe_stdin;
  ULONG writed;

  if( numargs != 2 )
    return INVALID_ROUTINE;

  sscanf( args[0].strptr, "%u:%u:%u", &pid, &pipe_stdout, &pipe_stdin );
  DosWrite( pipe_stdin, (PVOID)args[1].strptr, args[1].strlength, &writed );

  ltoa( writed, retstr->strptr, 10 );
  retstr->strlength = strlen( retstr->strptr );
  return VALID_ROUTINE;
}

/**
 * AwPKill( handle )
 *
 * Flags a process to end, and returns the termination code to
 * its parent (if any)
 */

ULONG AwPKill( PUCHAR    name,
               ULONG     numargs,
               RXSTRING  args[],
               PSZ       queuename,
               PRXSTRING retstr     )
{
  APIRET rc;
  PID    pid;
  HFILE  pipe_stdout, pipe_stdin;

  if( numargs != 1 )
    return INVALID_ROUTINE;

  sscanf( args[0].strptr, "%u:%u:%u", &pid, &pipe_stdout, &pipe_stdin );
  rc = DosKillProcess( 1, pid );

  ltoa( rc, retstr->strptr, 10 );
  retstr->strlength = strlen( retstr->strptr );
  return VALID_ROUTINE;
}

/**
 * AwPClose( handle )
 *
 * Waits completions of the program and releases all internal
 * resources connected to it. Returns an exit code of the
 * program.
 */

ULONG AwPClose( PUCHAR    name,
                ULONG     numargs,
                RXSTRING  args[],
                PSZ       queuename,
                PRXSTRING retstr     )
{
  APIRET rc;
  PID    pid;
  HFILE  pipe_stdout, pipe_stdin;

  RESULTCODES resCodes;

  if( numargs != 1 )
    return INVALID_ROUTINE;

  sscanf( args[0].strptr, "%u:%u:%u", &pid, &pipe_stdout, &pipe_stdin );
  rc = DosWaitChild( DCWA_PROCESS, DCWW_WAIT, &resCodes, &pid, pid );

  if( pipe_stdout ) DosClose( pipe_stdout );
  if( pipe_stdin  ) DosClose( pipe_stdin  );

  ltoa( resCodes.codeResult, retstr->strptr, 10 );
  retstr->strlength = strlen( retstr->strptr );
  return VALID_ROUTINE;
}

/**
 * AwQueryURL( msg_prompt, msg_add, msg_cancel, homes, url,
 *                         msg_username, msg_password )
 *
 * Displays the PM dialog for input of a URL. If in the
 * clipboard there is a correct URL - it is copied in dialog
 * entry field. Returns empty string in a case if the user
 * has cancelled input or the program is started in the PM
 * incompatible mode.
 */

ULONG AwQueryURL( PUCHAR    name,
                  ULONG     numargs,
                  RXSTRING  args[],
                  PSZ       queuename,
                  PRXSTRING retstr   )
{
  HAB      hab    = NULLHANDLE;
  HMQ      hmq    = NULLHANDLE;
  HWND     handle = NULLHANDLE;
  HMODULE  hdll   = NULLHANDLE;
  HWND     hlbox  = NULLHANDLE;
  SHVBLOCK shv;
  char*    str;
  ULONG    len;
  URL      url;

  char  stem[256], stem_name[256], stem_data[512];
  ULONG stem_count;

  PPIB ppib;
  DosGetInfoBlocks( NULL, &ppib );

  if( ppib->pib_ultype != 3 ) // Not PM
  {
    retstr->strlength = 0;
    return VALID_ROUTINE;
  }

  if( !( hab = WinInitialize(0)))
    return INVALID_ROUTINE;

  if( !( hmq = WinCreateMsgQueue( hab, 0 )) &&
       ( WinGetLastError( hab ) & 0xFFFF ) != PMERR_MSG_QUEUE_ALREADY_EXISTS )
  {
    return INVALID_ROUTINE;
  }

  if( DosQueryModuleHandle( "AWGET.DLL", &hdll ) != NO_ERROR )
    return INVALID_ROUTINE;
  if( !( handle = WinLoadDlg( HWND_DESKTOP, 0, WinDefDlgProc, hdll, ID_WIN_URL, 0 )))
    return INVALID_ROUTINE;
  if( !( hlbox  = WinWindowFromID( handle, ID_CMB_URL )))
    return INVALID_ROUTINE;

  if( numargs > 0 && RXVALIDSTRING( args[0] ))  // msg_prompt
    WinSetDlgItemText( handle, ID_LBL_URL, args[0].strptr );
  if( numargs > 1 && RXVALIDSTRING( args[1] ))  // msg_add
    WinSetDlgItemText( handle, DID_OK    , args[1].strptr );
  if( numargs > 2 && RXVALIDSTRING( args[2] ))  // msg_cancel
    WinSetDlgItemText( handle, DID_CANCEL, args[2].strptr );
  if( numargs > 5 && RXVALIDSTRING( args[5] ))  // msg_username
    WinSetDlgItemText( handle, ID_LBL_USR, args[5].strptr );
  if( numargs > 6 && RXVALIDSTRING( args[6] ))  // msg_password
    WinSetDlgItemText( handle, ID_LBL_PSW, args[6].strptr );

  if( numargs > 3 && RXVALIDSTRING( args[3] ))
  {
    // Have list of home folders
    strcpy( stem, args[3].strptr );

    if( stem[strlen(stem)-1] != '.' )
      strcat( stem, "." );

    shv.shvnext     = NULL;
    shv.shvcode     = RXSHV_SYFET;
    shv.shvret      = 0;
    shv.shvvaluelen = sizeof(stem_data);

    sprintf( stem_name, "%s0", stem );
    MAKERXSTRING( shv.shvname , stem_name, strlen(stem_name));
    MAKERXSTRING( shv.shvvalue, stem_data, sizeof(stem_data));

    if( RexxVariablePool( &shv ) != RXSHV_OK )
      return INVALID_ROUTINE;

    stem_count = atol( shv.shvvalue.strptr );
    while( stem_count-- )
    {
      shv.shvret = 0;
      sprintf( stem_name, "%s%d", stem, stem_count+1 );

      MAKERXSTRING( shv.shvname , stem_name, strlen(stem_name));
      MAKERXSTRING( shv.shvvalue, stem_data, sizeof(stem_data));

      if( RexxVariablePool( &shv ) != RXSHV_OK )
        return INVALID_ROUTINE;

      WinInsertLboxItem( hlbox, 0, shv.shvvalue.strptr );
    }

    WinSendMsg( hlbox, LM_SELECTITEM, 0, MPFROMLONG(TRUE));
  }

  if( numargs > 4 && RXVALIDSTRING( args[4] ))
  {
    // Have URL as argument
    url_allocate( &url, args[4].strptr );
  }
  else if( WinOpenClipbrd( hab ))
  {
    // Have URL in clipboard
    char* clipboard = (char*)WinQueryClipbrdData( hab, CF_TEXT );

    if( clipboard && ( strnicmp( clipboard, "HTTP://" , 7 ) == 0 ||
                       strnicmp( clipboard, "HTTPS://", 8 ) == 0 ||
                       strnicmp( clipboard, "FTP://"  , 6 ) == 0 ))
    {
      for( len = 0; clipboard[len]; len++ )
        if( clipboard[len+1] == '\r' || clipboard[len+1] == '\n' )
          break;

      str = (char*)malloc(len+1);
      strncpy( str, clipboard, len+1 );
      url_allocate( &url, str );
      free( str );
    }
    else
      url_allocate( &url, NULL );

    WinCloseClipbrd( hab );
  }
  else
  {
    // Don't have any URL
    url_allocate( &url, NULL );
  }

  if( url.username )
  {
    // Have user name in URL string
    WinSetDlgItemText( handle, ID_ENT_USR, url.username );
    free( url.username );
    url.username = NULL;
  }

  if( url.password )
  {
    // Have user password in URL string
    WinSetDlgItemText( handle, ID_ENT_PSW, url.password );
    free( url.password );
    url.password = NULL;
  }

  str = url_full( &url );
  len = strlen( str );

  if( len  )
  {
    // Sets URL in the dialog entry
    WinSetDlgItemText( handle, ID_ENT_URL, str );
    WinSendDlgItemMsg( handle, ID_ENT_URL, EM_SETSEL, MPFROM2SHORT( 0, len ), 0 );

    if( len > 50 )
      WinSendDlgItemMsg( handle, ID_ENT_URL, EM_SETFIRSTCHAR,
                                             MPFROMLONG( len - 40 ), 0 );
  }

  free( str );
  url_free( &url );

  if( WinProcessDlg( handle ) != DID_OK )
    retstr->strlength = 0;
  else
  {
    // Allocate URL structure
    len = WinQueryDlgItemTextLength( handle, ID_ENT_URL );
    str = (char*)malloc(len+1);

    WinQueryDlgItemText( handle, ID_ENT_URL, len+1, str );
    url_allocate( &url, str );
    free( str );

    // Replace user name if it is entered
    if(( len = WinQueryDlgItemTextLength( handle, ID_ENT_USR )) > 0 )
    {
      free( url.username );
      url.username = (char*)malloc(len+1);
      WinQueryDlgItemText( handle, ID_ENT_USR, len+1, url.username );
    }

    // Replace password if it is entered
    if(( len = WinQueryDlgItemTextLength( handle, ID_ENT_PSW )) > 0 )
    {
      free( url.password );
      url.password = (char*)malloc(len+1);
      WinQueryDlgItemText( handle, ID_ENT_PSW, len+1, url.password );
    }

    str = url_full( &url );
    len = strlen( str );

    if( DosAllocMem((PPVOID)&retstr->strptr, len, PAG_COMMIT | PAG_WRITE ))
      retstr->strlength = 0;
    else
    {
      retstr->strlength = len;
      strcpy( retstr->strptr, str );

      sprintf( stem_name, "%sselect", stem );
      sprintf( stem_data, "%u", WinQueryLboxSelectedItem( hlbox ));

      shv.shvcode     = RXSHV_SYSET;
      shv.shvret      = 0;
      shv.shvvaluelen = strlen(stem_data);

      MAKERXSTRING( shv.shvname , stem_name, strlen(stem_name));
      MAKERXSTRING( shv.shvvalue, stem_data, strlen(stem_data));

      if( RexxVariablePool( &shv ) == RXSHV_BADN )
        return INVALID_ROUTINE;
    }

    free( str );
    url_free( &url );
  }

  if( hmq ) WinDestroyMsgQueue( hmq );
  if( hab ) WinTerminate( hab );
  return VALID_ROUTINE;
}

/**
 * AwShow( show )
 *
 * You can show or hide the current session window. In case of failure,
 * one of many return codes is given so that the exact reason for
 * failure can be determined.
 */

ULONG AwShow( PUCHAR    name,
              ULONG     numargs,
              RXSTRING  args[],
              PSZ       queuename,
              PRXSTRING retstr   )
{
  BOOL    show;
  PPIB    ppib;
  HSWITCH hswitch;
  SWCNTRL cswitch;

  if( numargs != 1 )
    return INVALID_ROUTINE;

  DosGetInfoBlocks( NULL, &ppib );
  strcpy( retstr->strptr, "1" );

  hswitch = WinQuerySwitchHandle( NULLHANDLE, ppib->pib_ulpid );
  show    = atol( args[0].strptr );

  if( hswitch && WinQuerySwitchEntry( hswitch, &cswitch ) == NO_ERROR )
    if( WinShowWindow( cswitch.hwnd, 1 ))
      strcpy( retstr->strptr, "0" );

  retstr->strlength = strlen( retstr->strptr );
  return VALID_ROUTINE;
}

/**
 * AwPgmType( pathname )
 *
 * Returns the application type of an executable file,
 * possible values: UNKNOWN, VIO, FS, PM, DOS
 */

ULONG AwPgmType( PUCHAR    name,
                 ULONG     numargs,
                 RXSTRING  args[],
                 PSZ       queuename,
                 PRXSTRING retstr   )
{
  ULONG flags;

  if( numargs != 1 )
    return INVALID_ROUTINE;

  if( DosQueryAppType( args[0].strptr, &flags ) == NO_ERROR )
  {
    if( flags & FAPPTYP_DOS )
      strcpy( retstr->strptr, "DOS" );
    else
      switch( flags & FAPPTYP_EXETYPE )
      {
        case FAPPTYP_WINDOWAPI:
          strcpy( retstr->strptr, "PM" );
          break;
        case FAPPTYP_NOTWINDOWCOMPAT:
          strcpy( retstr->strptr, "FS" );
          break;
        case FAPPTYP_WINDOWCOMPAT:
          strcpy( retstr->strptr, "VIO" );
          break;
        default:
          strcpy( retstr->strptr, "UNKNOWN" );
      }
  }
  else
      strcpy( retstr->strptr, "UNKNOWN" );

  retstr->strlength = strlen( retstr->strptr );
  return VALID_ROUTINE;
}

/**
 * AwTimeStamp( pathname )
 *
 * Returns the date and time if last write for specified
 * file or directory in next format YYYYMMDD HHMMSS.
 */

ULONG AwTimeStamp( PUCHAR    name,
                   ULONG     numargs,
                   RXSTRING  args[],
                   PSZ       queuename,
                   PRXSTRING retstr   )
{
  FILESTATUS3 state;
  APIRET rc;

  if( numargs != 1 )
    return INVALID_ROUTINE;

  rc = DosQueryPathInfo( args[0].strptr, FIL_STANDARD, &state, sizeof(state));

  if( rc == NO_ERROR )
  {
    sprintf( retstr->strptr, "%04d%02d%02d %02d%02d%02d",
             state.fdateLastWrite.year + 1980,
             state.fdateLastWrite.month,
             state.fdateLastWrite.day,
             state.ftimeLastWrite.hours,
             state.ftimeLastWrite.minutes,
             state.ftimeLastWrite.twosecs * 2 );

    retstr->strlength = strlen( retstr->strptr );
  }
  else
    retstr->strlength = 0;

  return VALID_ROUTINE;
}

/**
 * AwIsPM()
 *
 * Returns TRUE (1) if the program executed in PM,
 * otherwise returns FALSE (0).
 */

ULONG AwIsPM( PUCHAR    name,
              ULONG     numargs,
              RXSTRING  args[],
              PSZ       queuename,
              PRXSTRING retstr   )
{
  PPIB ppib;
  DosGetInfoBlocks( NULL, &ppib );

  if( ppib->pib_ultype != 3 )
    strcpy( retstr->strptr, "0" );
  else
    strcpy( retstr->strptr, "1" );

  retstr->strlength = strlen( retstr->strptr );
  return VALID_ROUTINE;
}
