package lexer

import com.jetbrains.rider.ideaInterop.fileTypes.fsharp.lexer.*
import com.intellij.lexer.Lexer
import com.intellij.testFramework.LexerTestCase
import org.testng.annotations.Test

@Test
class FSharpLexerTest : LexerTestCase() {
    override fun createLexer(): Lexer {
        return FSharpLexer()
    }

    override fun getPathToTestDataFile(extension: String): String {
        return System.getProperty("user.dir") + "\\testData\\" + dirPath + "\\" + getTestName(true) + extension
    }

    override fun getDirPath(): String {
        return "lexing"
    }

    override fun getExpectedFileExtension() = ".gold"

    private fun doTest() {
        doFileTest("fs")
    }

    @Test fun testDigit() = doTest()
    @Test fun testString() = doTest()
    @Test fun testVerbatimString() = doTest()
    @Test fun testByteArray() = doTest()
    @Test fun testTripleQuotedString() = doTest()
    @Test fun testSymbolicOperator() = doTest()
    @Test fun testSimpleBlockComment() = doTest()
    @Test fun testBlockComment() = doTest()
    @Test fun testBlockCommentError() = doTest()
    @Test fun testSymbolicKeyword() = doTest()
    @Test fun testStringEscapeChar() = doTest()
    @Test fun testEscapeChar() = doTest()
    @Test fun testKeywordString() = doTest()
    @Test fun testUnfinishedTripleQuoteStringInComment() = doTest()
    @Test fun testIntDotDot() = doTest()
    @Test fun testIdent() = doTest()
    @Test fun testEndOfLineComment() = doTest()
    @Test fun testUnfinishedString() = doTest()
    @Test fun testCodeQuotation() = doTest()
    @Test fun testBadOperator() = doTest()
    @Test fun testAttribute() = doTest()
    @Test fun testTypeApp() = doTest()
    @Test fun testCorrectTypeApp() = doTest()
    @Test fun testIncorrectTypeApp() = doTest()
    @Test fun testGenericDeclaration() = doTest()
    @Test fun testIfDirective() = doTest()
    @Test fun testElseDirective() = doTest()
    @Test fun testEndIfDirective() = doTest()
    @Test fun testLightDirective() = doTest()
    @Test fun testLineDirective() = doTest()
    @Test fun testBadCommentInDirective() = doTest()
    @Test fun testEscapeCharacterInString() = doTest()
    @Test fun testEscapeCharacterInTripleQuotedString() = doTest()
    @Test fun testEscapeCharacterInVerbatimString() = doTest()
    @Test fun testInteractiveDirective() = doTest()
    @Test fun testHelpQuitDirective() = doTest()
    @Test fun testLoadDirective() = doTest()
    @Test fun testReferenceDirective() = doTest()
    @Test fun testTimeDirective() = doTest()
    @Test fun testIDirective() = doTest()
    @Test fun testSpaceDirective() = doTest()
    @Test fun testFlexibleType() = doTest()
    @Test fun testCharInString() = doTest()
    @Test fun testCharVerbatimString() = doTest()
    @Test fun testCharTripleQuoteString() = doTest()
    @Test fun testLeftArrow() = doTest()
    @Test fun testRightArrow() = doTest()
    @Test fun testBackslashInString() = doTest()
    @Test fun testCommentInTypeApp() = doTest()
    @Test fun testBlockCommentInTypeApp() = doTest()
    @Test fun testValidIdentifiers() = doTest()
    @Test fun testSmashingGreaterBarRBrack() = doTest()
}
