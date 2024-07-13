package com.dormitory;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@Slf4j
@SpringBootApplication
public class DormitoryManagementApplication {

    public static void main(String[] args) {
        SpringApplication.run(DormitoryManagementApplication.class, args);
        log.info("====================================项目启动成功,SpringBoot Version：[{}]====================================",
                SpringApplication.class.getPackage().getImplementationVersion());
    }

}
