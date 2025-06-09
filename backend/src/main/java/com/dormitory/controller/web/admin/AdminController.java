package com.dormitory.controller.web.admin;


import cn.dev33.satoken.stp.StpUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.dto.AdminDTO;
import com.dormitory.controller.dto.AdminLoginDTO;
import com.dormitory.controller.dto.ChangePasswordDTO;
import com.dormitory.controller.qry.AdminQry;
import com.dormitory.controller.vo.AdminLoginVO;
import com.dormitory.controller.vo.AdminVO;
import com.dormitory.service.SysAdminService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

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

    @ApiOperation("管理员分页查询")
    @PostMapping("/page")
//    @SaCheckPermission("admin:page")
    public R<IPage<AdminVO>> page(@RequestBody AdminQry qry) {
        return R.success(sysAdminService.pageByQry(qry));
    }

    @ApiOperation("管理员信息详情")
    @PostMapping("/detail/{adminId}")
//    @SaCheckPermission("admin:detail")
    public R<AdminVO> detail(@PathVariable @NotNull(message = "管理员ID") Long adminId) {
        return R.success(sysAdminService.detail(adminId));
    }
    
    @ApiOperation("新增管理员信息")
    @PostMapping("/add")
//    @SaCheckPermission("admin:add")
    public R<Boolean> add(@RequestBody @Valid AdminDTO dto) {
        return R.success(sysAdminService.add(dto));
    }

    @ApiOperation("编辑管理员信息")
    @PostMapping("/edit/{adminId}")
//    @SaCheckPermission("admin:edit")
    public R<Boolean> edit(@PathVariable @NotNull(message = "管理员ID") Long adminId, @RequestBody @Valid AdminDTO dto) {
        return R.success(sysAdminService.edit(adminId, dto));
    }

    @ApiOperation("删除管理员信息")
    @DeleteMapping("/del/{adminId}")
//    @SaCheckPermission("admin:del")
    public R<Boolean> del(@PathVariable @NotNull(message = "管理员ID") Long adminId) {
        return R.success(sysAdminService.del(adminId));
    }
}
