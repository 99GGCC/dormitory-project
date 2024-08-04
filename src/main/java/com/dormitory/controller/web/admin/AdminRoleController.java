package com.dormitory.controller.web.admin;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.dto.RoleDTO;
import com.dormitory.controller.qry.RoleQry;
import com.dormitory.controller.vo.RoleVO;
import com.dormitory.service.SysRoleService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * <p>
 * 角色表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-05-07
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/role")
@Api(value = "AdminRoleController", tags = {"角色信息接口"})
public class AdminRoleController {
    /**
     * 角色Service
     */
    private final SysRoleService roleService;

    @ApiOperation("角色分页查询")
    @GetMapping("/page")
//    @SaCheckPermission("role:page")
    public R<IPage<RoleVO>> page(RoleQry roleQry) {
        return R.success(roleService.pageByQry(roleQry));
    }

    @ApiOperation("角色详情")
    @GetMapping("/detail/{roleId}")
//    @SaCheckPermission("role:detail")
    public R<RoleVO> detail(@PathVariable @NotNull(message = "角色ID") Long roleId) {
        return R.success(roleService.detailById(roleId));
    }

    @ApiOperation("新增角色")
    @PostMapping("/save")
//    @SaCheckPermission("role:add")
    public R<Boolean> save(@RequestBody @Valid RoleDTO roleDTO) {
        return R.success(roleService.save(roleDTO));
    }

    @ApiOperation("修改角色")
    @PostMapping("/edit/{roleId}")
//    @SaCheckPermission("role:edit")
    public R<Boolean> edit(@NotNull(message = "角色ID") @PathVariable Long roleId,
                           @RequestBody @Valid RoleDTO roleDTO) {
        return R.success(roleService.edit(roleId, roleDTO));
    }

    @ApiOperation("删除角色")
    @DeleteMapping("/del/{roleId}")
//    @SaCheckPermission("role:del")
    public R<Boolean> del(@NotNull(message = "角色ID") @PathVariable Long roleId) {
        return R.success(roleService.del(roleId));
    }

    @ApiOperation("查询角色信息列表")
    @GetMapping("/list")
//    @SaCheckPermission("role:list")
    public R<List<RoleVO>> list() {
        return R.success(roleService.roleList());
    }
}

