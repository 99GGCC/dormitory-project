package com.dormitory.controller.web.admin;


import com.dormitory.common.R;
import com.dormitory.controller.dto.AdminLoginDTO;
import com.dormitory.controller.dto.ChangePasswordDTO;
import com.dormitory.controller.vo.AdminLoginVO;
import com.dormitory.controller.vo.AdminVO;
import com.dormitory.service.SysAdminService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

/**
 * <p>
 * 管理员表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin")
@Api(value = "AdminController", tags = {"管理员信息接口"})
public class AdminController {

    /**
     * 管理员Service
     */
    private final SysAdminService sysAdminService;

    /**
     * 管理员登录
     */
    @ApiOperation("管理员登录")
    @PostMapping("/login")
    public R<AdminLoginVO> login(@RequestBody @Valid AdminLoginDTO loginDTO) {
        return R.success(sysAdminService.login(loginDTO));
    }

    /**
     * 个人信息
     */
    @ApiOperation("个人信息")
    @GetMapping("/mine")
    public R<AdminVO> mine() {
        return R.success(sysAdminService.mine());
    }

    /**
     * 修改登录密码
     */
    @ApiOperation("修改登录密码")
    @PostMapping("/change/password")
    public R<Boolean> changePassword(@RequestBody @Valid ChangePasswordDTO changeDTO) {
        return R.success(sysAdminService.changePassword(changeDTO));
    }
}
