package com.dormitory.controller.web.admin;


import com.dormitory.common.R;
import com.dormitory.controller.dto.RoleMenuDTO;
import com.dormitory.controller.vo.RoleMenuVO;
import com.dormitory.service.SysRoleMenuService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * <p>
 * 角色菜单表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-05-07
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/role/menu")
@Api(value = "AdminRoleMenuController", tags = {"角色菜单接口"})
public class AdminRoleMenuController {
    /**
     * 角色菜单Service
     */
    private final SysRoleMenuService roleMenuService;

    @ApiOperation("根据角色ID获取菜单列表")
    @GetMapping("/tree/{roleId}")
//    @SaCheckPermission("role:tree")
    public R<List<RoleMenuVO>> listTree(@PathVariable @NotNull(message = "角色ID") Long roleId) {
        return R.success(roleMenuService.listTree(roleId));
    }

    @ApiOperation("角色授权菜单")
    @PostMapping("/empower")
//    @SaCheckPermission("role:empower")
    public R<Boolean> empower(@RequestBody @Valid RoleMenuDTO roleMenuDTO) {
        return R.success(roleMenuService.empower(roleMenuDTO));
    }

    @ApiOperation("查询登录用户菜单树")
    @GetMapping("/user")
    public R<List<RoleMenuVO>> user() {
        return R.success(roleMenuService.user());
    }

    @ApiOperation("查询登录用户列表")
    @GetMapping("/user/list")
    public R<List<RoleMenuVO>> userList() {
        return R.success(roleMenuService.userList());
    }
}

