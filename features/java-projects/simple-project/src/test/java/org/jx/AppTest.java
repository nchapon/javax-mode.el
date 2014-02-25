package org.jx;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;

/**
 * Unit test for simple App.
 */
public class AppTest {

    @Test
    public void should_returns_true() throws Exception {
        App app = new App();
        assertThat(app.sum(3,4)).isEqualTo(7);
    }

}
