package com.dormitory.controller.web.student;


import com.dormitory.common.R;
import com.dormitory.controller.dto.ChangePasswordDTO;
import com.dormitory.controller.dto.StudentLoginDTO;
import com.dormitory.controller.dto.StudentMineDTO;
import com.dormitory.controller.vo.StudentLoginVO;
import com.dormitory.controller.vo.StudentVO;
import com.dormitory.service.SysStudentService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

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

    /**
     * 根据学生ID获取登录信息
     */
    @ApiOperation("根据学生ID获取登录信息")
    @PostMapping("/token/{studentId}")
    public R<StudentLoginVO> token(@PathVariable Long studentId) {
        return R.success(sysStudentService.token(studentId));
    }

    /**
     * 个人信息
     */
    @ApiOperation("个人信息")
    @GetMapping("/mine")
    public R<StudentVO> mine() {
        return R.success(sysStudentService.mine());
    }

    /**
     * 修改个人信息
     */
    @ApiOperation("修改个人信息")
    @PostMapping("/edit")
    public R<Boolean> editMine(@RequestBody @Valid StudentMineDTO mineDTO) {
        return R.success(sysStudentService.editMine(mineDTO));
    }

    /**
     * 修改登录密码
     */
    @ApiOperation("修改登录密码")
    @PostMapping("/change/password")
    public R<Boolean> changePassword(@RequestBody @Valid ChangePasswordDTO changeDTO) {
        return R.success(sysStudentService.changePassword(changeDTO));
    }

}

