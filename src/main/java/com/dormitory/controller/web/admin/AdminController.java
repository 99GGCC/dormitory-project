package com.dormitory.controller.web.admin;


import com.dormitory.common.R;
import com.dormitory.controller.dto.AdminLoginDTO;
import com.dormitory.controller.vo.AdminLoginVO;
import com.dormitory.service.SysAdminService;
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
}
