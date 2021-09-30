package com.foo.rest.examples.spring.db;

import com.foo.rest.examples.spring.SpringController;
import com.p6spy.engine.spy.P6SpyDriver;
import kotlin.random.Random;
import org.evomaster.client.java.controller.db.DbCleaner;
import org.hibernate.dialect.H2Dialect;
import org.springframework.boot.SpringApplication;
import org.springframework.jdbc.core.JdbcTemplate;

import java.sql.Connection;
import java.sql.SQLException;

public class SpringWithDbController extends SpringController {

    static {
        /**
         * To avoid issues with non-determinism checks (in particular in the handling of taint-analysis),
         * we must disable the cache in H2
         */
        System.setProperty("h2.objectCache", "false");
    }

    protected Connection connection;

    protected SpringWithDbController(Class<?> applicationClass) {
        super(applicationClass);
    }


    @Override
    public String startSut() {

        //lot of problem if using same H2 instance. see:
        //https://github.com/h2database/h2database/issues/227
        int rand = Random.Default.nextInt();

        ctx = SpringApplication.run(applicationClass, new String[]{
                "--server.port=0",
                "--spring.datasource.url=jdbc:p6spy:h2:mem:testdb_"+rand+";DB_CLOSE_DELAY=-1;",
                "--spring.datasource.driver-class-name=" + P6SpyDriver.class.getName(),
                "--spring.jpa.database-platform=" + H2Dialect.class.getName(),
                "--spring.datasource.username=sa",
                "--spring.datasource.password",
                "--spring.jpa.properties.hibernate.show_sql=true"
        });


        if (connection != null) {
            try {
                connection.close();
            } catch (SQLException e) {
                e.printStackTrace();
            }
        }
        JdbcTemplate jdbc = ctx.getBean(JdbcTemplate.class);

        try {
            connection = jdbc.getDataSource().getConnection();
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }

        return "http://localhost:" + getSutPort();
    }

    @Override
    public void resetStateOfSUT() {
        if(connection != null) {
            DbCleaner.clearDatabase_H2(connection);
        }
    }

    @Override
    public void stopSut() {
        super.stopSut();
        connection = null;
    }

    @Override
    public Connection getConnection() {
        return connection;
    }

    @Override
    public String getDatabaseDriverName() {
        return "org.h2.Driver";
    }
}
