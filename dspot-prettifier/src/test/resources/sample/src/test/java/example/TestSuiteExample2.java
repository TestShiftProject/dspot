package example;

import org.junit.Assert;
import org.junit.Test;

public class TestSuiteExample2 {

    @Test(timeout = 10000)
    public void test2_mg63_assSep1() throws Exception {
        int __DSPOT_index_9 = -839241703;
        String __DSPOT_s_8 = "?i!rb0/|]6^FT)-ef&bk";
        Example ex = new Example();
        char findChar = ex.charAt("abcd", 3);
        char o_test2_mg63__7 = ex.charAt(__DSPOT_s_8, __DSPOT_index_9);
        Assert.assertEquals('?', ((Example) (ex)).charAt(__DSPOT_s_8, __DSPOT_index_9));
    }

    @Test(timeout = 10000)
    public void test2_mg63_assSep152() throws Exception {
        int __DSPOT_index_9 = -839241703;
        String __DSPOT_s_8 = "?i!rb0/|]6^FT)-ef&bk";
        Example ex = new Example();
        char findChar = ex.charAt("abcd", 3);
        char o_test2_mg63__7 = ex.charAt(__DSPOT_s_8, __DSPOT_index_9);
        Assert.assertEquals('?', (char) o_test2_mg63__7);
    }
}