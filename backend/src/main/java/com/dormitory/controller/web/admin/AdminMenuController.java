package com.dormitory.controller.web.admin;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.qry.MenuQry;
import com.dormitory.controller.vo.MenuVO;
import com.dormitory.service.SysMenuService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.constraints.NotNull;

/**
 * <p>
 * 菜单表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-05-07
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/menu")
@Api(value = "AdminMenuController", tags = {"菜单信息接口"})
public class AdminMenuController {
    /**
     * 菜单Service
     */
    private final SysMenuService menuService;

    @ApiOperation("菜单分页查询")
    @GetMapping("/page")
//    @SaCheckPermission("menu:page")
    public R<IPage<MenuVO>> page(MenuQry menuQry) {
        return R.success(menuService.pageByQry(menuQry));
    }

    @ApiOperation("菜单详情")
    @GetMapping("/detail/{menuId}")
//    @SaCheckPermission("menu:detail")
    public R<MenuVO> detail(@PathVariable @NotNull(message = "菜单ID") Long menuId) {
        return R.success(menuService.detailById(menuId));
    }
}

