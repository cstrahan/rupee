{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.Rupee.Builtins
  (
  -- * Classes
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

import Foreign.Rupee.CAPI
import Foreign.Ptr
import Foreign.Storable
import Control.Applicative
import Control.Monad
import Control.Monad.Trans


-- Classes
rbBasicObject :: RBIO RValue
rbBasicObject = RBIO $ lift $ peek <$> rb_cBasicObject

rbObject :: RBIO RValue
rbObject = RBIO $ lift $ peek <$> rb_cObject

rbArray :: RBIO RValue
rbArray = RBIO $ lift $ peek <$> rb_cArray

rbBignum :: RBIO RValue
rbBignum = RBIO $ lift $ peek <$> rb_cBignum

rbBinding :: RBIO RValue
rbBinding = RBIO $ lift $ peek <$> rb_cBinding

rbClass :: RBIO RValue
rbClass = RBIO $ lift $ peek <$> rb_cClass

rbCont :: RBIO RValue
rbCont = RBIO $ lift $ peek <$> rb_cCont

rbDir :: RBIO RValue
rbDir = RBIO $ lift $ peek <$> rb_cDir

rbData :: RBIO RValue
rbData = RBIO $ lift $ peek <$> rb_cData

rbFalseClass :: RBIO RValue
rbFalseClass = RBIO $ lift $ peek <$> rb_cFalseClass

rbEncoding :: RBIO RValue
rbEncoding = RBIO $ lift $ peek <$> rb_cEncoding

rbEnumerator :: RBIO RValue
rbEnumerator = RBIO $ lift $ peek <$> rb_cEnumerator

rbFile :: RBIO RValue
rbFile = RBIO $ lift $ peek <$> rb_cFile

rbFixnum :: RBIO RValue
rbFixnum = RBIO $ lift $ peek <$> rb_cFixnum

rbFloat :: RBIO RValue
rbFloat = RBIO $ lift $ peek <$> rb_cFloat

rbHash :: RBIO RValue
rbHash = RBIO $ lift $ peek <$> rb_cHash

rbInteger :: RBIO RValue
rbInteger = RBIO $ lift $ peek <$> rb_cInteger

rbIO :: RBIO RValue
rbIO = RBIO $ lift $ peek <$> rb_cIO

rbMatch :: RBIO RValue
rbMatch = RBIO $ lift $ peek <$> rb_cMatch

rbMethod :: RBIO RValue
rbMethod = RBIO $ lift $ peek <$> rb_cMethod

rbModule :: RBIO RValue
rbModule = RBIO $ lift $ peek <$> rb_cModule

rbNameErrorMesg :: RBIO RValue
rbNameErrorMesg = RBIO $ lift $ peek <$> rb_cNameErrorMesg

rbNilClass :: RBIO RValue
rbNilClass = RBIO $ lift $ peek <$> rb_cNilClass

rbNumeric :: RBIO RValue
rbNumeric = RBIO $ lift $ peek <$> rb_cNumeric

rbProc :: RBIO RValue
rbProc = RBIO $ lift $ peek <$> rb_cProc

rbRandom :: RBIO RValue
rbRandom = RBIO $ lift $ peek <$> rb_cRandom

rbRange :: RBIO RValue
rbRange = RBIO $ lift $ peek <$> rb_cRange

rbRational :: RBIO RValue
rbRational = RBIO $ lift $ peek <$> rb_cRational

rbComplex :: RBIO RValue
rbComplex = RBIO $ lift $ peek <$> rb_cComplex

rbRegexp :: RBIO RValue
rbRegexp = RBIO $ lift $ peek <$> rb_cRegexp

rbStat :: RBIO RValue
rbStat = RBIO $ lift $ peek <$> rb_cStat

rbString :: RBIO RValue
rbString = RBIO $ lift $ peek <$> rb_cString

rbStruct :: RBIO RValue
rbStruct = RBIO $ lift $ peek <$> rb_cStruct

rbSymbol :: RBIO RValue
rbSymbol = RBIO $ lift $ peek <$> rb_cSymbol

rbThread :: RBIO RValue
rbThread = RBIO $ lift $ peek <$> rb_cThread

rbTime :: RBIO RValue
rbTime = RBIO $ lift $ peek <$> rb_cTime

rbTrueClass :: RBIO RValue
rbTrueClass = RBIO $ lift $ peek <$> rb_cTrueClass

rbUnboundMethod :: RBIO RValue
rbUnboundMethod = RBIO $ lift $ peek <$> rb_cUnboundMethod


-- Modules
rbKernel :: RBIO RValue
rbKernel = RBIO $ lift $ peek <$> rb_mKernel

rbComparable :: RBIO RValue
rbComparable = RBIO $ lift $ peek <$> rb_mComparable

rbEnumerable :: RBIO RValue
rbEnumerable = RBIO $ lift $ peek <$> rb_mEnumerable

rbErrno :: RBIO RValue
rbErrno = RBIO $ lift $ peek <$> rb_mErrno

rbFileTest :: RBIO RValue
rbFileTest = RBIO $ lift $ peek <$> rb_mFileTest

rbGC :: RBIO RValue
rbGC = RBIO $ lift $ peek <$> rb_mGC

rbMath :: RBIO RValue
rbMath = RBIO $ lift $ peek <$> rb_mMath

rbProcess :: RBIO RValue
rbProcess = RBIO $ lift $ peek <$> rb_mProcess

rbWaitReadable :: RBIO RValue
rbWaitReadable = RBIO $ lift $ peek <$> rb_mWaitReadable

rbWaitWritable :: RBIO RValue
rbWaitWritable = RBIO $ lift $ peek <$> rb_mWaitWritable


-- Exceptions
rbException :: RBIO RValue
rbException = RBIO $ lift $ peek <$> rb_eException

rbStandardError :: RBIO RValue
rbStandardError = RBIO $ lift $ peek <$> rb_eStandardError

rbSystemExit :: RBIO RValue
rbSystemExit = RBIO $ lift $ peek <$> rb_eSystemExit

rbInterrupt :: RBIO RValue
rbInterrupt = RBIO $ lift $ peek <$> rb_eInterrupt

rbSignal :: RBIO RValue
rbSignal = RBIO $ lift $ peek <$> rb_eSignal

rbFatal :: RBIO RValue
rbFatal = RBIO $ lift $ peek <$> rb_eFatal

rbArgError :: RBIO RValue
rbArgError = RBIO $ lift $ peek <$> rb_eArgError

rbEOFError :: RBIO RValue
rbEOFError = RBIO $ lift $ peek <$> rb_eEOFError

rbIndexError :: RBIO RValue
rbIndexError = RBIO $ lift $ peek <$> rb_eIndexError

rbStopIteration :: RBIO RValue
rbStopIteration = RBIO $ lift $ peek <$> rb_eStopIteration

rbKeyError :: RBIO RValue
rbKeyError = RBIO $ lift $ peek <$> rb_eKeyError

rbRangeError :: RBIO RValue
rbRangeError = RBIO $ lift $ peek <$> rb_eRangeError

rbIOError :: RBIO RValue
rbIOError = RBIO $ lift $ peek <$> rb_eIOError

rbRuntimeError :: RBIO RValue
rbRuntimeError = RBIO $ lift $ peek <$> rb_eRuntimeError

rbSecurityError :: RBIO RValue
rbSecurityError = RBIO $ lift $ peek <$> rb_eSecurityError

rbSystemCallError :: RBIO RValue
rbSystemCallError = RBIO $ lift $ peek <$> rb_eSystemCallError

rbThreadError :: RBIO RValue
rbThreadError = RBIO $ lift $ peek <$> rb_eThreadError

rbTypeError :: RBIO RValue
rbTypeError = RBIO $ lift $ peek <$> rb_eTypeError

rbZeroDivError :: RBIO RValue
rbZeroDivError = RBIO $ lift $ peek <$> rb_eZeroDivError

rbNotImpError :: RBIO RValue
rbNotImpError = RBIO $ lift $ peek <$> rb_eNotImpError

rbNoMemError :: RBIO RValue
rbNoMemError = RBIO $ lift $ peek <$> rb_eNoMemError

rbNoMethodError :: RBIO RValue
rbNoMethodError = RBIO $ lift $ peek <$> rb_eNoMethodError

rbFloatDomainError :: RBIO RValue
rbFloatDomainError = RBIO $ lift $ peek <$> rb_eFloatDomainError

rbLocalJumpError :: RBIO RValue
rbLocalJumpError = RBIO $ lift $ peek <$> rb_eLocalJumpError

rbSysStackError :: RBIO RValue
rbSysStackError = RBIO $ lift $ peek <$> rb_eSysStackError

rbRegexpError :: RBIO RValue
rbRegexpError = RBIO $ lift $ peek <$> rb_eRegexpError

rbEncodingError :: RBIO RValue
rbEncodingError = RBIO $ lift $ peek <$> rb_eEncodingError

rbEncCompatError :: RBIO RValue
rbEncCompatError = RBIO $ lift $ peek <$> rb_eEncCompatError

rbScriptError :: RBIO RValue
rbScriptError = RBIO $ lift $ peek <$> rb_eScriptError

rbNameError :: RBIO RValue
rbNameError = RBIO $ lift $ peek <$> rb_eNameError

rbSyntaxError :: RBIO RValue
rbSyntaxError = RBIO $ lift $ peek <$> rb_eSyntaxError

rbLoadError :: RBIO RValue
rbLoadError = RBIO $ lift $ peek <$> rb_eLoadError

rbMathDomainError :: RBIO RValue
rbMathDomainError = RBIO $ lift $ peek <$> rb_eMathDomainError


-- stdin, stdout, stderr
rbstdin :: RBIO RValue
rbstdin = RBIO $ lift $ peek <$> rb_stdin

rbstdout :: RBIO RValue
rbstdout = RBIO $ lift $ peek <$> rb_stdin

rbstderr :: RBIO RValue
rbstderr = RBIO $ lift $ peek <$> rb_stdin


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
