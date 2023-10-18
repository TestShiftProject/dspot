package eu.stamp_project;

import org.junit.Assert;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class AppTest {

    @Test
    public void test1() {
        App app = new App(4);
        assertEquals(4, app.getInt());
        app.compute();
        assertEquals(8, app.getInt());
        app.compute(10);
        assertEquals(40, app.getInt());
        app.compute(3);
        assertEquals(60, app.getInt());
    }

    @Test
    public void test2_failAssert0() throws Exception {
        // AssertionGenerator generate try/catch block with fail statement
        try {
            App app = new App(4);
            app.throwException();
            Assert.fail("test2 should have thrown IllegalArgumentException");
        } catch (IllegalArgumentException expected) {
            Assert.assertEquals("Illegal Arg", expected.getMessage());
        }
    }

    @Test(timeout = 10000)
    public void test1_mg12_assSep41() throws Exception {
        int __DSPOT_j_0 = -1150482841;
        App app = new App(4);
        app.compute(__DSPOT_j_0);
        Assert.assertEquals(-153482034, ((int) (((App) (app)).getInt())));
    }

    @Test(timeout = 10000)
    public void test1_mg13_failAssert0() throws Exception {
        try {
            App app = new App(4);
            app.throwException();
            Assert.fail("test1_mg13 should have thrown IllegalArgumentException");
        } catch (IllegalArgumentException expected) {
            Assert.assertEquals("Illegal Arg", expected.getMessage());
        }
    }

    // the content of the following few tests doesn't matter, only the additional coverage define in the report is
    // used for filtering
    @Test(timeout = 10000)
    public void testSimpleGetter() throws Exception {
        // this test should be filtered out, as it only adds coverage in a simple getter
        Assert.assertTrue(true);
    }

    @Test(timeout = 10000)
    public void testSimpleBooleanGetter() throws Exception {
        // this test should be filtered out, as it only adds coverage in a simple getter
        Assert.assertTrue(true);
    }

    @Test(timeout = 10000)
    public void testException() throws Exception {
        // this test should be left in, even if it only adds coverage in a simple getter, because it tests an
        // exception according to the modification report
        Assert.assertTrue(true);
    }

    @Test(timeout = 10000)
    public void testHashCode() throws Exception {
        // this test should be filtered as it adds coverage in hashcode, which we can't test in a useful way at the
        // moment
        Assert.assertTrue(true);
    }

    // used for prioritization
    @Test(timeout = 10000)
    public void testAddingOneLineOfCoverageInOneMethod() throws Exception {
        // this test should be left in, even if it only adds coverage in a simple getter, because it tests an
        // exception according to the modification report
        Assert.assertTrue(true);
    }

    @Test(timeout = 10000)
    public void testAddingTwoLinesOfCoverageInOneMethod() throws Exception {
        // this test should be filtered as it adds coverage in hashcode, which we can't test in a useful way at the
        // moment
        Assert.assertTrue(true);
    }

    @Test(timeout = 10000)
    public void testAddingOneLineOfCoverageInTwoMethods() throws Exception {
        // this test should be filtered as it adds coverage in hashcode, which we can't test in a useful way at the
        // moment
        Assert.assertTrue(true);
    }
}