package org.jx.system;



import java.util.List;
import org.slf4j.LoggerFactory;
import java.util.ArrayList;
import org.slf4j.Logger;





public class Calculator {

    private static Logger logger = LoggerFactory.getLogger(Calculator.class);

    public int sum (int a, int b) {
        logger.info("Sum {} {}",a,b);
        List list = new ArrayList();
        return a+b;
    }

}
