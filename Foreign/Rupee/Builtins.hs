{-# LANGUAGE ForeignFunctionInterface #-}

-- THIS IS A GENERATED FILE
--
-- DO NOT EDIT!

module Foreign.Rupee.Builtins
  ( -- * Classes
    rbBasicObject
  , rbObject
  , rbArray
  , rbBignum
  , rbBinding
  , rbClass
  , rbCont
  , rbDir
  , rbData
  , rbFalseClass
  , rbEncoding
  , rbEnumerator
  , rbFile
  , rbFixnum
  , rbFloat
  , rbHash
  , rbInteger
  , rbIO
  , rbMatch
  , rbMethod
  , rbModule
  , rbNameErrorMesg
  , rbNilClass
  , rbNumeric
  , rbProc
  , rbRandom
  , rbRange
  , rbRational
  , rbComplex
  , rbRegexp
  , rbStat
  , rbString
  , rbStruct
  , rbSymbol
  , rbThread
  , rbTime
  , rbTrueClass
  , rbUnboundMethod
    -- * Modules
  , rbKernel
  , rbComparable
  , rbEnumerable
  , rbErrno
  , rbFileTest
  , rbGC
  , rbMath
  , rbProcess
  , rbWaitReadable
  , rbWaitWritable
    -- * Exceptions
  , rbException
  , rbStandardError
  , rbSystemExit
  , rbInterrupt
  , rbSignal
  , rbFatal
  , rbArgError
  , rbEOFError
  , rbIndexError
  , rbStopIteration
  , rbKeyError
  , rbRangeError
  , rbIOError
  , rbRuntimeError
  , rbSecurityError
  , rbSystemCallError
  , rbThreadError
  , rbTypeError
  , rbZeroDivError
  , rbNotImpError
  , rbNoMemError
  , rbNoMethodError
  , rbFloatDomainError
  , rbLocalJumpError
  , rbSysStackError
  , rbRegexpError
  , rbEncodingError
  , rbEncCompatError
  , rbScriptError
  , rbNameError
  , rbSyntaxError
  , rbLoadError
  , rbMathDomainError
    -- * stdin, stdout, stderr
  , rbstdin
  , rbstdout
  , rbstderr
  ) where

import Foreign.Rupee.Types
import Foreign.Ptr
import Foreign.Storable
import Control.Applicative
import Control.Monad
import Control.Monad.Trans


-- Classes
rbBasicObject :: Ruby RValue
rbBasicObject = lift $ peek rb_cBasicObject

rbObject :: Ruby RValue
rbObject = lift $ peek rb_cObject

rbArray :: Ruby RValue
rbArray = lift $ peek rb_cArray

rbBignum :: Ruby RValue
rbBignum = lift $ peek rb_cBignum

rbBinding :: Ruby RValue
rbBinding = lift $ peek rb_cBinding

rbClass :: Ruby RValue
rbClass = lift $ peek rb_cClass

rbCont :: Ruby RValue
rbCont = lift $ peek rb_cCont

rbDir :: Ruby RValue
rbDir = lift $ peek rb_cDir

rbData :: Ruby RValue
rbData = lift $ peek rb_cData

rbFalseClass :: Ruby RValue
rbFalseClass = lift $ peek rb_cFalseClass

rbEncoding :: Ruby RValue
rbEncoding = lift $ peek rb_cEncoding

rbEnumerator :: Ruby RValue
rbEnumerator = lift $ peek rb_cEnumerator

rbFile :: Ruby RValue
rbFile = lift $ peek rb_cFile

rbFixnum :: Ruby RValue
rbFixnum = lift $ peek rb_cFixnum

rbFloat :: Ruby RValue
rbFloat = lift $ peek rb_cFloat

rbHash :: Ruby RValue
rbHash = lift $ peek rb_cHash

rbInteger :: Ruby RValue
rbInteger = lift $ peek rb_cInteger

rbIO :: Ruby RValue
rbIO = lift $ peek rb_cIO

rbMatch :: Ruby RValue
rbMatch = lift $ peek rb_cMatch

rbMethod :: Ruby RValue
rbMethod = lift $ peek rb_cMethod

rbModule :: Ruby RValue
rbModule = lift $ peek rb_cModule

rbNameErrorMesg :: Ruby RValue
rbNameErrorMesg = lift $ peek rb_cNameErrorMesg

rbNilClass :: Ruby RValue
rbNilClass = lift $ peek rb_cNilClass

rbNumeric :: Ruby RValue
rbNumeric = lift $ peek rb_cNumeric

rbProc :: Ruby RValue
rbProc = lift $ peek rb_cProc

rbRandom :: Ruby RValue
rbRandom = lift $ peek rb_cRandom

rbRange :: Ruby RValue
rbRange = lift $ peek rb_cRange

rbRational :: Ruby RValue
rbRational = lift $ peek rb_cRational

rbComplex :: Ruby RValue
rbComplex = lift $ peek rb_cComplex

rbRegexp :: Ruby RValue
rbRegexp = lift $ peek rb_cRegexp

rbStat :: Ruby RValue
rbStat = lift $ peek rb_cStat

rbString :: Ruby RValue
rbString = lift $ peek rb_cString

rbStruct :: Ruby RValue
rbStruct = lift $ peek rb_cStruct

rbSymbol :: Ruby RValue
rbSymbol = lift $ peek rb_cSymbol

rbThread :: Ruby RValue
rbThread = lift $ peek rb_cThread

rbTime :: Ruby RValue
rbTime = lift $ peek rb_cTime

rbTrueClass :: Ruby RValue
rbTrueClass = lift $ peek rb_cTrueClass

rbUnboundMethod :: Ruby RValue
rbUnboundMethod = lift $ peek rb_cUnboundMethod


-- Modules
rbKernel :: Ruby RValue
rbKernel = lift $ peek rb_mKernel

rbComparable :: Ruby RValue
rbComparable = lift $ peek rb_mComparable

rbEnumerable :: Ruby RValue
rbEnumerable = lift $ peek rb_mEnumerable

rbErrno :: Ruby RValue
rbErrno = lift $ peek rb_mErrno

rbFileTest :: Ruby RValue
rbFileTest = lift $ peek rb_mFileTest

rbGC :: Ruby RValue
rbGC = lift $ peek rb_mGC

rbMath :: Ruby RValue
rbMath = lift $ peek rb_mMath

rbProcess :: Ruby RValue
rbProcess = lift $ peek rb_mProcess

rbWaitReadable :: Ruby RValue
rbWaitReadable = lift $ peek rb_mWaitReadable

rbWaitWritable :: Ruby RValue
rbWaitWritable = lift $ peek rb_mWaitWritable


-- Exceptions
rbException :: Ruby RValue
rbException = lift $ peek rb_eException

rbStandardError :: Ruby RValue
rbStandardError = lift $ peek rb_eStandardError

rbSystemExit :: Ruby RValue
rbSystemExit = lift $ peek rb_eSystemExit

rbInterrupt :: Ruby RValue
rbInterrupt = lift $ peek rb_eInterrupt

rbSignal :: Ruby RValue
rbSignal = lift $ peek rb_eSignal

rbFatal :: Ruby RValue
rbFatal = lift $ peek rb_eFatal

rbArgError :: Ruby RValue
rbArgError = lift $ peek rb_eArgError

rbEOFError :: Ruby RValue
rbEOFError = lift $ peek rb_eEOFError

rbIndexError :: Ruby RValue
rbIndexError = lift $ peek rb_eIndexError

rbStopIteration :: Ruby RValue
rbStopIteration = lift $ peek rb_eStopIteration

rbKeyError :: Ruby RValue
rbKeyError = lift $ peek rb_eKeyError

rbRangeError :: Ruby RValue
rbRangeError = lift $ peek rb_eRangeError

rbIOError :: Ruby RValue
rbIOError = lift $ peek rb_eIOError

rbRuntimeError :: Ruby RValue
rbRuntimeError = lift $ peek rb_eRuntimeError

rbSecurityError :: Ruby RValue
rbSecurityError = lift $ peek rb_eSecurityError

rbSystemCallError :: Ruby RValue
rbSystemCallError = lift $ peek rb_eSystemCallError

rbThreadError :: Ruby RValue
rbThreadError = lift $ peek rb_eThreadError

rbTypeError :: Ruby RValue
rbTypeError = lift $ peek rb_eTypeError

rbZeroDivError :: Ruby RValue
rbZeroDivError = lift $ peek rb_eZeroDivError

rbNotImpError :: Ruby RValue
rbNotImpError = lift $ peek rb_eNotImpError

rbNoMemError :: Ruby RValue
rbNoMemError = lift $ peek rb_eNoMemError

rbNoMethodError :: Ruby RValue
rbNoMethodError = lift $ peek rb_eNoMethodError

rbFloatDomainError :: Ruby RValue
rbFloatDomainError = lift $ peek rb_eFloatDomainError

rbLocalJumpError :: Ruby RValue
rbLocalJumpError = lift $ peek rb_eLocalJumpError

rbSysStackError :: Ruby RValue
rbSysStackError = lift $ peek rb_eSysStackError

rbRegexpError :: Ruby RValue
rbRegexpError = lift $ peek rb_eRegexpError

rbEncodingError :: Ruby RValue
rbEncodingError = lift $ peek rb_eEncodingError

rbEncCompatError :: Ruby RValue
rbEncCompatError = lift $ peek rb_eEncCompatError

rbScriptError :: Ruby RValue
rbScriptError = lift $ peek rb_eScriptError

rbNameError :: Ruby RValue
rbNameError = lift $ peek rb_eNameError

rbSyntaxError :: Ruby RValue
rbSyntaxError = lift $ peek rb_eSyntaxError

rbLoadError :: Ruby RValue
rbLoadError = lift $ peek rb_eLoadError

rbMathDomainError :: Ruby RValue
rbMathDomainError = lift $ peek rb_eMathDomainError


-- stdin, stdout, stderr
rbstdin :: Ruby RValue
rbstdin = lift $ peek rb_stdin

rbstdout :: Ruby RValue
rbstdout = lift $ peek rb_stdout

rbstderr :: Ruby RValue
rbstderr = lift $ peek rb_stderr


-- Foreign decls
foreign import ccall "&rb_mKernel"           rb_mKernel           :: Ptr RValue
foreign import ccall "&rb_mComparable"       rb_mComparable       :: Ptr RValue
foreign import ccall "&rb_mEnumerable"       rb_mEnumerable       :: Ptr RValue
foreign import ccall "&rb_mErrno"            rb_mErrno            :: Ptr RValue
foreign import ccall "&rb_mFileTest"         rb_mFileTest         :: Ptr RValue
foreign import ccall "&rb_mGC"               rb_mGC               :: Ptr RValue
foreign import ccall "&rb_mMath"             rb_mMath             :: Ptr RValue
foreign import ccall "&rb_mProcess"          rb_mProcess          :: Ptr RValue
foreign import ccall "&rb_mWaitReadable"     rb_mWaitReadable     :: Ptr RValue
foreign import ccall "&rb_mWaitWritable"     rb_mWaitWritable     :: Ptr RValue

foreign import ccall "&rb_cBasicObject"      rb_cBasicObject      :: Ptr RValue
foreign import ccall "&rb_cObject"           rb_cObject           :: Ptr RValue
foreign import ccall "&rb_cArray"            rb_cArray            :: Ptr RValue
foreign import ccall "&rb_cBignum"           rb_cBignum           :: Ptr RValue
foreign import ccall "&rb_cBinding"          rb_cBinding          :: Ptr RValue
foreign import ccall "&rb_cClass"            rb_cClass            :: Ptr RValue
foreign import ccall "&rb_cCont"             rb_cCont             :: Ptr RValue
foreign import ccall "&rb_cDir"              rb_cDir              :: Ptr RValue
foreign import ccall "&rb_cData"             rb_cData             :: Ptr RValue
foreign import ccall "&rb_cFalseClass"       rb_cFalseClass       :: Ptr RValue
foreign import ccall "&rb_cEncoding"         rb_cEncoding         :: Ptr RValue
foreign import ccall "&rb_cEnumerator"       rb_cEnumerator       :: Ptr RValue
foreign import ccall "&rb_cFile"             rb_cFile             :: Ptr RValue
foreign import ccall "&rb_cFixnum"           rb_cFixnum           :: Ptr RValue
foreign import ccall "&rb_cFloat"            rb_cFloat            :: Ptr RValue
foreign import ccall "&rb_cHash"             rb_cHash             :: Ptr RValue
foreign import ccall "&rb_cInteger"          rb_cInteger          :: Ptr RValue
foreign import ccall "&rb_cIO"               rb_cIO               :: Ptr RValue
foreign import ccall "&rb_cMatch"            rb_cMatch            :: Ptr RValue
foreign import ccall "&rb_cMethod"           rb_cMethod           :: Ptr RValue
foreign import ccall "&rb_cModule"           rb_cModule           :: Ptr RValue
foreign import ccall "&rb_cNameErrorMesg"    rb_cNameErrorMesg    :: Ptr RValue
foreign import ccall "&rb_cNilClass"         rb_cNilClass         :: Ptr RValue
foreign import ccall "&rb_cNumeric"          rb_cNumeric          :: Ptr RValue
foreign import ccall "&rb_cProc"             rb_cProc             :: Ptr RValue
foreign import ccall "&rb_cRandom"           rb_cRandom           :: Ptr RValue
foreign import ccall "&rb_cRange"            rb_cRange            :: Ptr RValue
foreign import ccall "&rb_cRational"         rb_cRational         :: Ptr RValue
foreign import ccall "&rb_cComplex"          rb_cComplex          :: Ptr RValue
foreign import ccall "&rb_cRegexp"           rb_cRegexp           :: Ptr RValue
foreign import ccall "&rb_cStat"             rb_cStat             :: Ptr RValue
foreign import ccall "&rb_cString"           rb_cString           :: Ptr RValue
foreign import ccall "&rb_cStruct"           rb_cStruct           :: Ptr RValue
foreign import ccall "&rb_cSymbol"           rb_cSymbol           :: Ptr RValue
foreign import ccall "&rb_cThread"           rb_cThread           :: Ptr RValue
foreign import ccall "&rb_cTime"             rb_cTime             :: Ptr RValue
foreign import ccall "&rb_cTrueClass"        rb_cTrueClass        :: Ptr RValue
foreign import ccall "&rb_cUnboundMethod"    rb_cUnboundMethod    :: Ptr RValue

foreign import ccall "&rb_eException"        rb_eException        :: Ptr RValue
foreign import ccall "&rb_eStandardError"    rb_eStandardError    :: Ptr RValue
foreign import ccall "&rb_eSystemExit"       rb_eSystemExit       :: Ptr RValue
foreign import ccall "&rb_eInterrupt"        rb_eInterrupt        :: Ptr RValue
foreign import ccall "&rb_eSignal"           rb_eSignal           :: Ptr RValue
foreign import ccall "&rb_eFatal"            rb_eFatal            :: Ptr RValue
foreign import ccall "&rb_eArgError"         rb_eArgError         :: Ptr RValue
foreign import ccall "&rb_eEOFError"         rb_eEOFError         :: Ptr RValue
foreign import ccall "&rb_eIndexError"       rb_eIndexError       :: Ptr RValue
foreign import ccall "&rb_eStopIteration"    rb_eStopIteration    :: Ptr RValue
foreign import ccall "&rb_eKeyError"         rb_eKeyError         :: Ptr RValue
foreign import ccall "&rb_eRangeError"       rb_eRangeError       :: Ptr RValue
foreign import ccall "&rb_eIOError"          rb_eIOError          :: Ptr RValue
foreign import ccall "&rb_eRuntimeError"     rb_eRuntimeError     :: Ptr RValue
foreign import ccall "&rb_eSecurityError"    rb_eSecurityError    :: Ptr RValue
foreign import ccall "&rb_eSystemCallError"  rb_eSystemCallError  :: Ptr RValue
foreign import ccall "&rb_eThreadError"      rb_eThreadError      :: Ptr RValue
foreign import ccall "&rb_eTypeError"        rb_eTypeError        :: Ptr RValue
foreign import ccall "&rb_eZeroDivError"     rb_eZeroDivError     :: Ptr RValue
foreign import ccall "&rb_eNotImpError"      rb_eNotImpError      :: Ptr RValue
foreign import ccall "&rb_eNoMemError"       rb_eNoMemError       :: Ptr RValue
foreign import ccall "&rb_eNoMethodError"    rb_eNoMethodError    :: Ptr RValue
foreign import ccall "&rb_eFloatDomainError" rb_eFloatDomainError :: Ptr RValue
foreign import ccall "&rb_eLocalJumpError"   rb_eLocalJumpError   :: Ptr RValue
foreign import ccall "&rb_eSysStackError"    rb_eSysStackError    :: Ptr RValue
foreign import ccall "&rb_eRegexpError"      rb_eRegexpError      :: Ptr RValue
foreign import ccall "&rb_eEncodingError"    rb_eEncodingError    :: Ptr RValue
foreign import ccall "&rb_eEncCompatError"   rb_eEncCompatError   :: Ptr RValue

foreign import ccall "&rb_eScriptError"      rb_eScriptError      :: Ptr RValue
foreign import ccall "&rb_eNameError"        rb_eNameError        :: Ptr RValue
foreign import ccall "&rb_eSyntaxError"      rb_eSyntaxError      :: Ptr RValue
foreign import ccall "&rb_eLoadError"        rb_eLoadError        :: Ptr RValue

foreign import ccall "&rb_eMathDomainError"  rb_eMathDomainError  :: Ptr RValue

foreign import ccall "&rb_stdin"             rb_stdin             :: Ptr RValue
foreign import ccall "&rb_stdout"            rb_stdout            :: Ptr RValue
foreign import ccall "&rb_stderr"            rb_stderr            :: Ptr RValue
