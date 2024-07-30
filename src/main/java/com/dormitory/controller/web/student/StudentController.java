package com.dormitory.controller.web.student;


import com.dormitory.common.R;
import com.dormitory.controller.dto.StudentLoginDTO;
import com.dormitory.controller.vo.AdminLoginVO;
import com.dormitory.controller.vo.StudentLoginVO;
import com.dormitory.service.SysStudentService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

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
@RequestMapping("/student")
@Api(value = "StudentController", tags = {"学生信息接口"})
public class StudentController {

    /**
     * 学生信息Service
     */
    private final SysStudentService sysStudentService;


    /**
     * 学生登录
     */
    @ApiOperation("学生登录")
    @PostMapping("/login")
    public R<StudentLoginVO> login(@RequestBody @Valid StudentLoginDTO loginDTO) {
        return R.success(sysStudentService.login(loginDTO));
    }
}

