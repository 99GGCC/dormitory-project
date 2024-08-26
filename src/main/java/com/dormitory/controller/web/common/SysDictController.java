package com.dormitory.controller.web.common;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.dto.ClassesInfoDTO;
import com.dormitory.controller.dto.SysDictDTO;
import com.dormitory.controller.qry.ClassesInfoQry;
import com.dormitory.controller.qry.SysDictQry;
import com.dormitory.controller.vo.SysDictVO;
import com.dormitory.service.SysDictService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * <p>
 * 字典表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/dict")
@Api(value = "SysDictController", tags = {"字典接口"})
public class SysDictController {

    /**
     * 字典信息Service
     */
    private final SysDictService sysDictService;

    @ApiOperation("字典信息分页查询")
    @GetMapping("/page")
    public R<IPage<SysDictVO>> pageByQry(SysDictQry qry) {
        return R.success(sysDictService.pageByQry(qry));
    }

    @ApiOperation("字典信息列表查询")
    @GetMapping("/list")
    public R<List<SysDictVO>> listByQry(SysDictQry qry) {
        return R.success(sysDictService.listByQry(qry));
    }


    @ApiOperation("字典信息缓存")
    @GetMapping("/cache")
    public R<Object> cache() {
        return R.success(sysDictService.cache());
    }



    @ApiOperation("字典信息详情")
    @GetMapping("/detail/{dictId}")
    public R<SysDictVO> detail(@PathVariable @NotNull(message = "字典ID") Long dictId) {
        return R.success(sysDictService.detailById(dictId));
    }

    @ApiOperation("新增字典信息")
    @PostMapping("/add")
    public R<Boolean> add(@RequestBody @Valid SysDictDTO dto) {
        return R.success(sysDictService.add(dto));
    }

    @ApiOperation("编辑字典信息")
    @PostMapping("/edit/{dictId}")
    public R<Boolean> edit(@NotNull(message = "字典ID") @PathVariable Long dictId,
                           @RequestBody @Valid SysDictDTO dto) {
        return R.success(sysDictService.edit(dictId, dto));
    }

    @ApiOperation("删除字典信息")
    @PostMapping("/del/{dictId}")
    public R<Boolean> del(@NotNull(message = "字典ID") @PathVariable Long dictId) {
        return R.success(sysDictService.del(dictId));
    }
}

