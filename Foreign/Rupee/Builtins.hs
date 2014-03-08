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

import Foreign.Rupee.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Control.Applicative
import Control.Monad
import Control.Monad.Trans


-- Classes
rbBasicObject :: IO (ForeignPtr RValue)
rbBasicObject = return rb_cBasicObject >>= newForeignPtr_

rbObject :: IO (ForeignPtr RValue)
rbObject = return rb_cObject >>= newForeignPtr_

rbArray :: IO (ForeignPtr RValue)
rbArray = return rb_cArray >>= newForeignPtr_

rbBignum :: IO (ForeignPtr RValue)
rbBignum = return rb_cBignum >>= newForeignPtr_

rbBinding :: IO (ForeignPtr RValue)
rbBinding = return rb_cBinding >>= newForeignPtr_

rbClass :: IO (ForeignPtr RValue)
rbClass = return rb_cClass >>= newForeignPtr_

rbCont :: IO (ForeignPtr RValue)
rbCont = return rb_cCont >>= newForeignPtr_

rbDir :: IO (ForeignPtr RValue)
rbDir = return rb_cDir >>= newForeignPtr_

rbData :: IO (ForeignPtr RValue)
rbData = return rb_cData >>= newForeignPtr_

rbFalseClass :: IO (ForeignPtr RValue)
rbFalseClass = return rb_cFalseClass >>= newForeignPtr_

rbEncoding :: IO (ForeignPtr RValue)
rbEncoding = return rb_cEncoding >>= newForeignPtr_

rbEnumerator :: IO (ForeignPtr RValue)
rbEnumerator = return rb_cEnumerator >>= newForeignPtr_

rbFile :: IO (ForeignPtr RValue)
rbFile = return rb_cFile >>= newForeignPtr_

rbFixnum :: IO (ForeignPtr RValue)
rbFixnum = return rb_cFixnum >>= newForeignPtr_

rbFloat :: IO (ForeignPtr RValue)
rbFloat = return rb_cFloat >>= newForeignPtr_

rbHash :: IO (ForeignPtr RValue)
rbHash = return rb_cHash >>= newForeignPtr_

rbInteger :: IO (ForeignPtr RValue)
rbInteger = return rb_cInteger >>= newForeignPtr_

rbIO :: IO (ForeignPtr RValue)
rbIO = return rb_cIO >>= newForeignPtr_

rbMatch :: IO (ForeignPtr RValue)
rbMatch = return rb_cMatch >>= newForeignPtr_

rbMethod :: IO (ForeignPtr RValue)
rbMethod = return rb_cMethod >>= newForeignPtr_

rbModule :: IO (ForeignPtr RValue)
rbModule = return rb_cModule >>= newForeignPtr_

rbNameErrorMesg :: IO (ForeignPtr RValue)
rbNameErrorMesg = return rb_cNameErrorMesg >>= newForeignPtr_

rbNilClass :: IO (ForeignPtr RValue)
rbNilClass = return rb_cNilClass >>= newForeignPtr_

rbNumeric :: IO (ForeignPtr RValue)
rbNumeric = return rb_cNumeric >>= newForeignPtr_

rbProc :: IO (ForeignPtr RValue)
rbProc = return rb_cProc >>= newForeignPtr_

rbRandom :: IO (ForeignPtr RValue)
rbRandom = return rb_cRandom >>= newForeignPtr_

rbRange :: IO (ForeignPtr RValue)
rbRange = return rb_cRange >>= newForeignPtr_

rbRational :: IO (ForeignPtr RValue)
rbRational = return rb_cRational >>= newForeignPtr_

rbComplex :: IO (ForeignPtr RValue)
rbComplex = return rb_cComplex >>= newForeignPtr_

rbRegexp :: IO (ForeignPtr RValue)
rbRegexp = return rb_cRegexp >>= newForeignPtr_

rbStat :: IO (ForeignPtr RValue)
rbStat = return rb_cStat >>= newForeignPtr_

rbString :: IO (ForeignPtr RValue)
rbString = return rb_cString >>= newForeignPtr_

rbStruct :: IO (ForeignPtr RValue)
rbStruct = return rb_cStruct >>= newForeignPtr_

rbSymbol :: IO (ForeignPtr RValue)
rbSymbol = return rb_cSymbol >>= newForeignPtr_

rbThread :: IO (ForeignPtr RValue)
rbThread = return rb_cThread >>= newForeignPtr_

rbTime :: IO (ForeignPtr RValue)
rbTime = return rb_cTime >>= newForeignPtr_

rbTrueClass :: IO (ForeignPtr RValue)
rbTrueClass = return rb_cTrueClass >>= newForeignPtr_

rbUnboundMethod :: IO (ForeignPtr RValue)
rbUnboundMethod = return rb_cUnboundMethod >>= newForeignPtr_


-- Modules
rbKernel :: IO (ForeignPtr RValue)
rbKernel = return rb_mKernel >>= newForeignPtr_

rbComparable :: IO (ForeignPtr RValue)
rbComparable = return rb_mComparable >>= newForeignPtr_

rbEnumerable :: IO (ForeignPtr RValue)
rbEnumerable = return rb_mEnumerable >>= newForeignPtr_

rbErrno :: IO (ForeignPtr RValue)
rbErrno = return rb_mErrno >>= newForeignPtr_

rbFileTest :: IO (ForeignPtr RValue)
rbFileTest = return rb_mFileTest >>= newForeignPtr_

rbGC :: IO (ForeignPtr RValue)
rbGC = return rb_mGC >>= newForeignPtr_

rbMath :: IO (ForeignPtr RValue)
rbMath = return rb_mMath >>= newForeignPtr_

rbProcess :: IO (ForeignPtr RValue)
rbProcess = return rb_mProcess >>= newForeignPtr_

rbWaitReadable :: IO (ForeignPtr RValue)
rbWaitReadable = return rb_mWaitReadable >>= newForeignPtr_

rbWaitWritable :: IO (ForeignPtr RValue)
rbWaitWritable = return rb_mWaitWritable >>= newForeignPtr_


-- Exceptions
rbException :: IO (ForeignPtr RValue)
rbException = return rb_eException >>= newForeignPtr_

rbStandardError :: IO (ForeignPtr RValue)
rbStandardError = return rb_eStandardError >>= newForeignPtr_

rbSystemExit :: IO (ForeignPtr RValue)
rbSystemExit = return rb_eSystemExit >>= newForeignPtr_

rbInterrupt :: IO (ForeignPtr RValue)
rbInterrupt = return rb_eInterrupt >>= newForeignPtr_

rbSignal :: IO (ForeignPtr RValue)
rbSignal = return rb_eSignal >>= newForeignPtr_

rbFatal :: IO (ForeignPtr RValue)
rbFatal = return rb_eFatal >>= newForeignPtr_

rbArgError :: IO (ForeignPtr RValue)
rbArgError = return rb_eArgError >>= newForeignPtr_

rbEOFError :: IO (ForeignPtr RValue)
rbEOFError = return rb_eEOFError >>= newForeignPtr_

rbIndexError :: IO (ForeignPtr RValue)
rbIndexError = return rb_eIndexError >>= newForeignPtr_

rbStopIteration :: IO (ForeignPtr RValue)
rbStopIteration = return rb_eStopIteration >>= newForeignPtr_

rbKeyError :: IO (ForeignPtr RValue)
rbKeyError = return rb_eKeyError >>= newForeignPtr_

rbRangeError :: IO (ForeignPtr RValue)
rbRangeError = return rb_eRangeError >>= newForeignPtr_

rbIOError :: IO (ForeignPtr RValue)
rbIOError = return rb_eIOError >>= newForeignPtr_

rbRuntimeError :: IO (ForeignPtr RValue)
rbRuntimeError = return rb_eRuntimeError >>= newForeignPtr_

rbSecurityError :: IO (ForeignPtr RValue)
rbSecurityError = return rb_eSecurityError >>= newForeignPtr_

rbSystemCallError :: IO (ForeignPtr RValue)
rbSystemCallError = return rb_eSystemCallError >>= newForeignPtr_

rbThreadError :: IO (ForeignPtr RValue)
rbThreadError = return rb_eThreadError >>= newForeignPtr_

rbTypeError :: IO (ForeignPtr RValue)
rbTypeError = return rb_eTypeError >>= newForeignPtr_

rbZeroDivError :: IO (ForeignPtr RValue)
rbZeroDivError = return rb_eZeroDivError >>= newForeignPtr_

rbNotImpError :: IO (ForeignPtr RValue)
rbNotImpError = return rb_eNotImpError >>= newForeignPtr_

rbNoMemError :: IO (ForeignPtr RValue)
rbNoMemError = return rb_eNoMemError >>= newForeignPtr_

rbNoMethodError :: IO (ForeignPtr RValue)
rbNoMethodError = return rb_eNoMethodError >>= newForeignPtr_

rbFloatDomainError :: IO (ForeignPtr RValue)
rbFloatDomainError = return rb_eFloatDomainError >>= newForeignPtr_

rbLocalJumpError :: IO (ForeignPtr RValue)
rbLocalJumpError = return rb_eLocalJumpError >>= newForeignPtr_

rbSysStackError :: IO (ForeignPtr RValue)
rbSysStackError = return rb_eSysStackError >>= newForeignPtr_

rbRegexpError :: IO (ForeignPtr RValue)
rbRegexpError = return rb_eRegexpError >>= newForeignPtr_

rbEncodingError :: IO (ForeignPtr RValue)
rbEncodingError = return rb_eEncodingError >>= newForeignPtr_

rbEncCompatError :: IO (ForeignPtr RValue)
rbEncCompatError = return rb_eEncCompatError >>= newForeignPtr_

rbScriptError :: IO (ForeignPtr RValue)
rbScriptError = return rb_eScriptError >>= newForeignPtr_

rbNameError :: IO (ForeignPtr RValue)
rbNameError = return rb_eNameError >>= newForeignPtr_

rbSyntaxError :: IO (ForeignPtr RValue)
rbSyntaxError = return rb_eSyntaxError >>= newForeignPtr_

rbLoadError :: IO (ForeignPtr RValue)
rbLoadError = return rb_eLoadError >>= newForeignPtr_

rbMathDomainError :: IO (ForeignPtr RValue)
rbMathDomainError = return rb_eMathDomainError >>= newForeignPtr_


-- stdin, stdout, stderr
rbstdin :: IO (ForeignPtr RValue)
rbstdin =  return rb_stdin >>= newForeignPtr_

rbstdout :: IO (ForeignPtr RValue)
rbstdout =  return rb_stdout >>= newForeignPtr_

rbstderr :: IO (ForeignPtr RValue)
rbstderr =  return rb_stderr >>= newForeignPtr_


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
