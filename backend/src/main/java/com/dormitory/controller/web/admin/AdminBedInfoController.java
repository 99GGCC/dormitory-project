package com.dormitory.controller.web.admin;


import com.dormitory.common.R;
import com.dormitory.controller.dto.ArrangeBedDTO;
import com.dormitory.controller.dto.BedInfoDTO;
import com.dormitory.controller.dto.ReleaseBedDTO;
import com.dormitory.controller.qry.BedInfoQry;
import com.dormitory.controller.vo.BedInfoVO;
import com.dormitory.service.BedInfoService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * <p>
 * 床位信息表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/bed/info")
@Api(value = "AdminBedInfoController", tags = {"床位信息接口"})
public class AdminBedInfoController {

    /**
     * 床位信息Service
     */
    private final BedInfoService bedInfoService;

    @ApiOperation("床位信息列表查询")
    @GetMapping("/list")
    public R<List<BedInfoVO>> listByQry(BedInfoQry qry) {
        return R.success(bedInfoService.listByQry(qry));
    }

    @ApiOperation("新增床位信息")
    @PostMapping("/add")
    public R<Boolean> add(@RequestBody @Valid BedInfoDTO dto) {
        return R.success(bedInfoService.add(dto));
    }

    @ApiOperation("编辑床位信息")
    @PostMapping("/edit/{bedId}")
    public R<Boolean> edit(@NotNull(message = "床位ID") @PathVariable Long bedId,
                           @RequestBody @Valid BedInfoDTO dto) {
        return R.success(bedInfoService.edit(bedId, dto));
    }

    @ApiOperation("删除床位信息")
    @PostMapping("/del/{bedId}")
    public R<Boolean> del(@NotNull(message = "床位ID") @PathVariable Long bedId) {
        return R.success(bedInfoService.del(bedId));
    }

    @ApiOperation("安排床位")
    @PostMapping("/arrange")
    public R<Boolean> arrange(@Valid @RequestBody ArrangeBedDTO dto) {
        return R.success(bedInfoService.arrange(dto));
    }

    @ApiOperation("释放床位")
    @PostMapping("/release")
    public R<Boolean> release(@Valid @RequestBody ReleaseBedDTO dto) {
        return R.success(bedInfoService.release(dto));
    }


}

