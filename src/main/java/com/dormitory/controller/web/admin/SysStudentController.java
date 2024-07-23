package com.dormitory.controller.web.admin;


import com.dormitory.service.SysStudentService;
import io.swagger.annotations.Api;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * <p>
 * 学生信息表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/student")
@Api(value = "SysStudentController", tags = {"学生信息接口"})
public class SysStudentController {

    /**
     * 学生信息Service
     */
    private final SysStudentService sysStudentService;


}

